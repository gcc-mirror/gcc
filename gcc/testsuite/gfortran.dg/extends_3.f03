! { dg-do run }
! A test of f2k style constructors with derived type extension.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module persons
  type :: person
    character(24) :: name = ""
    integer :: ss = 1
  end type person
end module persons

module person_education
  use persons
  type, extends(person) :: education
    integer ::  attainment = 0
    character(24) :: institution = ""
  end type education
end module person_education

  use person_education
  type, extends(education) :: service
    integer :: personnel_number = 0
    character(24) :: department = ""
  end type service

  type, extends(service) :: person_record
    type (person_record), pointer :: supervisor => NULL ()
  end type person_record

  type(person_record), pointer :: recruit, supervisor
  
! Check that F2K constructor with missing entries works
  allocate (supervisor)
  supervisor%service = service (NAME = "Joe Honcho", SS= 123455)

  recruit => entry ("John Smith", 123456, 1, "Bog Hill High School", &
                    99, "Records", supervisor)

  if (supervisor%ss /= 123455) STOP 1
  if (trim (supervisor%name) /= "Joe Honcho") STOP 2
  if (trim (supervisor%institution) /= "") STOP 3
  if (supervisor%attainment /= 0) STOP 4

  if (trim (recruit%name) /= "John Smith") STOP 5
  if (recruit%name /= recruit%service%name) STOP 6
  if (recruit%supervisor%ss /= 123455) STOP 7
  if (recruit%supervisor%ss /= supervisor%person%ss) STOP 8

  deallocate (supervisor)
  deallocate (recruit)
contains
  function entry (name, ss, attainment, institution, &
                  personnel_number, department, supervisor) result (new_person)
    integer :: ss, attainment, personnel_number
    character (*) :: name, institution, department
    type (person_record), pointer :: supervisor, new_person

    allocate (new_person)

! Check F2K constructor with order shuffled a bit
    new_person = person_record (NAME = name, SS =ss, &
                                DEPARTMENT = department, &
                                INSTITUTION = institution, &
                                PERSONNEL_NUMBER = personnel_number, &
                                ATTAINMENT = attainment, &
                                SUPERVISOR = supervisor)
  end function
end
