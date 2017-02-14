! { dg-do run }
!
! Tests dtio transfer of arrays of derived types and classes
!
MODULE p
  TYPE :: person
    CHARACTER (LEN=20) :: name
    INTEGER(4) :: age
    CONTAINS
      procedure :: pwf
      procedure :: prf
      GENERIC :: WRITE(FORMATTED) => pwf
      GENERIC :: READ(FORMATTED) => prf
  END TYPE person
  type, extends(person) :: employee
    character(20) :: job_title
  end type
  type, extends(person) :: officer
    character(20) :: position
  end type
  type, extends(person) :: member
    integer :: membership_number
  end type
  type :: club
    type(employee), allocatable :: staff(:)
    class(person), allocatable :: committee(:)
    class(person), allocatable :: membership(:)
  end type
CONTAINS
  SUBROUTINE pwf (dtv,unit,iotype,vlist,iostat,iomsg)
    CLASS(person), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    select type (dtv)
      type is (employee)
        WRITE(unit, FMT = "(A/)", IOSTAT=iostat) "Employee"
        WRITE(unit, FMT = "(A20,I4,A20/)", IOSTAT=iostat) dtv%name, dtv%age, dtv%job_title
      type is (officer)
        WRITE(unit, FMT = "(A/)", IOSTAT=iostat) "Officer"
        WRITE(unit, FMT = "(A20,I4,A20/)", IOSTAT=iostat) dtv%name, dtv%age, dtv%position
      type is (member)
        WRITE(unit, FMT = "(A/)", IOSTAT=iostat) "Member"
        WRITE(unit, FMT = "(A20,I4,I4/)", IOSTAT=iostat) dtv%name, dtv%age, dtv%membership_number
      class default
        WRITE(unit, FMT = "(A/)", IOSTAT=iostat) "Ugggh!"
        WRITE(unit, FMT = "(A20,I4,' '/)", IOSTAT=iostat) dtv%name, dtv%age
    end select
  END SUBROUTINE pwf

  SUBROUTINE prf (dtv,unit,iotype,vlist,iostat,iomsg)
    CLASS(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER (LEN=*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: vlist(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    character (20) :: header, rname, jtitle, oposition
    integer :: i
    integer :: no
    integer :: age
    iostat = 0
    select type (dtv)

      type is (employee)
        read (unit = unit, fmt = *) header
        READ (UNIT = UNIT, FMT = "(A20,I4,A20)") rname, age, jtitle
        if (trim (rname) .ne. dtv%name) iostat = 1
        if (age .ne. dtv%age) iostat = 2
        if (trim (jtitle) .ne. dtv%job_title) iostat = 3
        if (iotype .ne. "DTstaff") iostat = 4

      type is (officer)
        read (unit = unit, fmt = *) header
        READ (UNIT = UNIT, FMT = "(A20,I4,A20)") rname, age, oposition
        if (trim (rname) .ne. dtv%name) iostat = 1
        if (age .ne. dtv%age) iostat = 2
        if (trim (oposition) .ne. dtv%position) iostat = 3
        if (iotype .ne. "DTofficers") iostat = 4

      type is (member)
        read (unit = unit, fmt = *) header
        READ (UNIT = UNIT, FMT = "(A20,I4,I4)") rname, age, no
        if (trim (rname) .ne. dtv%name) iostat = 1
        if (age .ne. dtv%age) iostat = 2
        if (no .ne. dtv%membership_number) iostat = 3
        if (iotype .ne. "DTmembers") iostat = 4

      class default
        call abort
    end select
  end subroutine
END MODULE p

PROGRAM test
  USE p

  type (club) :: social_club
  TYPE (person) :: chairman
  CLASS (person), allocatable :: president(:)
  character (40) :: line
  integer :: i, j

  allocate (social_club%staff, source = [employee ("Bert",25,"Barman"), &
                                         employee ("Joy",16,"Auditor")])

  allocate (social_club%committee, source = [officer ("Hank",32, "Chair"), &
                                             officer ("Ann", 29, "Secretary")])

  allocate (social_club%membership, source = [member ("Dan",52,1), &
                                              member ("Sue",39,2)])

  chairman%name="Charlie"
  chairman%age=62

  open (7, status = "scratch")
  write (7,*) social_club%staff                ! Tests array of derived types
  write (7,*) social_club%committee            ! Tests class array
  do i = 1, size (social_club%membership, 1)
    write (7,*) social_club%membership(i)      ! Tests class array elements
  end do

  rewind (7)
  read (7, "(DT'staff')", iostat = i) social_club%staff
  if (i .ne. 0) call abort

  social_club%committee(2)%age = 33            ! Introduce an error

  read (7, "(DT'officers')", iostat = i) social_club%committee
  if (i .ne. 2) call abort                     ! Pick up error

  do j = 1, size (social_club%membership, 1)
    read (7, "(DT'members')", iostat = i) social_club%membership(j)
    if (i .ne. 0) call abort
  end do
  close (7)
END PROGRAM test
