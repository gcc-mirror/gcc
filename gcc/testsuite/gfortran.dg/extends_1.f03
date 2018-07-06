! { dg-do run }
! A basic functional test of derived type extension.
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
  
! Check that references by ultimate component work

  allocate (supervisor)
  supervisor%name = "Joe Honcho"
  supervisor%ss = 123455
  supervisor%attainment = 100
  supervisor%institution = "Celestial University"
  supervisor%personnel_number = 1
  supervisor%department = "Directorate"

  recruit => entry ("John Smith", 123456, 1, "Bog Hill High School", &
                    99, "Records", supervisor)

  if (trim (recruit%name) /= "John Smith") STOP 1
  if (recruit%name /= recruit%service%name) STOP 2
  if (recruit%supervisor%ss /= 123455) STOP 3
  if (recruit%supervisor%ss /= supervisor%person%ss) STOP 4

  deallocate (supervisor)
  deallocate (recruit)
contains
  function entry (name, ss, attainment, institution, &
                  personnel_number, department, supervisor) result (new_person)
    integer :: ss, attainment, personnel_number
    character (*) :: name, institution, department
    type (person_record), pointer :: supervisor, new_person

    allocate (new_person)

! Check mixtures of references
    new_person%person%name = name
    new_person%service%education%person%ss = ss
    new_person%service%attainment = attainment
    new_person%education%institution = institution
    new_person%personnel_number = personnel_number
    new_person%service%department = department
    new_person%supervisor => supervisor
  end function
end
