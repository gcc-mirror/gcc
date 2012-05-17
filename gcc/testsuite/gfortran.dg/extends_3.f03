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

  if (supervisor%ss /= 123455) call abort
  if (trim (supervisor%name) /= "Joe Honcho") call abort
  if (trim (supervisor%institution) /= "") call abort
  if (supervisor%attainment /= 0) call abort

  if (trim (recruit%name) /= "John Smith") call abort
  if (recruit%name /= recruit%service%name) call abort
  if (recruit%supervisor%ss /= 123455) call abort
  if (recruit%supervisor%ss /= supervisor%person%ss) call abort

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
