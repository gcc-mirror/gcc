! { dg-do compile }
!
! PR 55959: [OOP] ICE in in gfc_simplify_expr, at fortran/expr.c:1920
!
! Contributed by Tilo Schwarz <tilo@tilo-schwarz.de>

module pdfs
  type :: pdf
  contains
    procedure, nopass :: getx
  end type

contains

  real function getx()
  end function

end module

program abstract
  use pdfs
  type(pdf) pp
  print pp%getx()  ! { dg-error "must be of type default-kind CHARACTER or of INTEGER" }
end program
