! { dg-do run }
! A test of f95 style constructors with derived type extension.
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

! Check that simple constructor works
  allocate (supervisor)
  supervisor%service = service ("Joe Honcho", 123455, 100, &
                                "Celestial University", 1, &
                                "Directorate")

  recruit => entry ("John Smith", 123456, 1, "Bog Hill High School", &
                    99, "Records", supervisor)

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

! Check nested constructors
    new_person = person_record (education (person (name, ss), &
                                attainment, institution), &
                                personnel_number, department, &
                                supervisor)
  end function
end

! { dg-final { cleanup-modules "persons person_education" } }
