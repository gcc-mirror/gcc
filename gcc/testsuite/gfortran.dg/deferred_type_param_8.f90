! { dg-do run }
!
! PR fortran/53642
! PR fortran/45170 (comments 24, 34, 37)
!

PROGRAM helloworld
  implicit none
  character(:),allocatable::string
  character(11), parameter :: cmp = "hello world"
  real::rnd
  integer :: n, i
  do i = 1, 10
     call random_number(rnd)
     n = ceiling(11*rnd)
     call hello(n, string)
!     print '(A,1X,I0)', '>' // string // '<', len(string)
     if (n /= len (string) .or. string /= cmp(1:n)) call abort ()
  end do

  call test_PR53642()

contains

  subroutine hello (n,string)
    character(:), allocatable, intent(out) :: string
    integer,intent(in) :: n
    character(11) :: helloworld="hello world"

    string=helloworld(:n)                       ! Didn't  work
!    string=(helloworld(:n))                    ! Works.
!    allocate(string, source=helloworld(:n))    ! Fixed for allocate_with_source_2.f90
!    allocate(string, source=(helloworld(:n)))  ! Works.
  end subroutine hello

  subroutine test_PR53642()
    character(len=4) :: string="123 "
    character(:), allocatable :: trimmed

    trimmed = trim(string)
    if (len_trim(string) /= len(trimmed)) call abort ()
    if (len(trimmed) /= 3) call abort ()
    if (trimmed /= "123") call abort ()
!    print *,len_trim(string),len(trimmed)

    ! Clear
    trimmed = "XXXXXX"
    if (trimmed /= "XXXXXX" .or. len(trimmed) /= 6) call abort ()

    trimmed = string(1:len_trim(string))
    if (len_trim(trimmed) /= 3) call abort ()
    if (trimmed /= "123") call abort ()
  end subroutine test_PR53642
end PROGRAM helloworld
