! { dg-do run  }
! { dg-options -std=gnu }
! PR55117  Programs fails namelist read (contains derived types objects)
program test_type_extension

  type tk_t
     real :: x
  end type tk_t

  type, extends(tk_t) :: tke_t
     character(8) :: string
  end type tke_t

  type, extends(tke_t) :: deep
    integer :: int1
    real :: y
    character(10) :: the_name
  end type deep

  type other
    integer :: one_oh
    integer :: two_oh
  end type other

  type plain_type
    integer :: var1
    type(other) :: var2
    real :: var3
  end type plain_type

  type some_other
    complex :: varx
    type(tke_t) :: tke
    type (plain_type) :: varpy
    real  :: vary
  end type some_other

  type(deep) :: trouble
  type(some_other) :: somethinelse
  type(tke_t) :: tke
  integer :: answer
  
  namelist /test_NML/ trouble, somethinelse, tke, answer

  tke%x = 0.0
  tke%string = "xxxxxxxx"
  answer = 5
  trouble%x = 5.34
  trouble%y = 4.25
  trouble%string = "yyyy"
  trouble%the_name = "mischief"

  open(10, status="scratch")
  
  write(10,*) "&TEST_NML"
  write(10,*) "TKE%X=  3.14    ,"
  write(10,*) "TKE%STRING='kf7rcc',"
  write(10,*) "ANSWER=          42,"
  write(10,*) "/"
  rewind(10)
  
  read(10,NML=test_NML)
  if (tke%x - 3.14000010 > .00001) call abort
  if (tke%string /= "kf7rcc") call abort
  if (answer /= 42) call abort ! hitchkikers guide to the galaxy
end program test_type_extension
