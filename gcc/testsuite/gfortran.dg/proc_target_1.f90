! { dg-do run }
!
! PR fortran/117070 - Procedure targets in derived-type constructors
!
! Contributed by Ivan Pribec

module funcs
  implicit none

  abstract interface
     function retchar()
       character(len=1) :: retchar
     end function retchar
  end interface
contains
  function a()
    character(len=1) :: a
    a = 'a'
  end function
  function b()
      character(len=1) :: b
      b = 'b'
  end function
  function c()
      character(len=1) :: c
      c = 'c'
  end function
end module

module dispatch_table
  use funcs
  implicit none

  ! Procedure container
  type :: pc
     procedure(retchar), pointer, nopass :: rc => null()
  end type pc

  type(pc), parameter :: dtab_p(3)  = [pc(a),pc(b),pc(c)] ! Parameter
  type(pc)            :: dtab_v(3)  = [pc(a),pc(b),pc(c)] ! Variable

contains

  ! Dynamic dispatch table
  function build_table() result(table)
    type(pc) :: table(3)
    table = [pc(a),pc(b),pc(c)]
  end function build_table

end module

program test
  use dispatch_table
  implicit none
  type(pc), parameter :: table_p(3)  = [pc(a),pc(b),pc(c)] ! Parameter
  type(pc)            :: table_v(3)  = [pc(a),pc(b),pc(c)] ! Variable
  type(pc) :: table(3)

  ! Get dispatch table from local variable
  table = table_v
  associate (abc => table(1)%rc()//table(2)%rc()//table(3)%rc())
    if (abc /= 'abc') stop 1
  end associate

  associate (abc => table_v(1)%rc()//table_v(2)%rc()//table_v(3)%rc())
    if (abc /= 'abc') stop 2
  end associate

  table = table_p
  associate (abc => table(1)%rc()//table(2)%rc()//table(3)%rc())
    if (abc /= 'abc') stop 3
  end associate

! Bogus error:
! "Operands of string concatenation operator at (1) are PROCEDURE/PROCEDURE"
! associate (abc => table_p(1)%rc()//table_p(2)%rc()//table_p(3)%rc())
!   if (abc /= 'abc') stop 4
! end associate

  ! Get dispatch table from other module and passed via local variable
  table = build_table() ! Dynamic table
  associate (abc => table(1)%rc()//table(2)%rc()//table(3)%rc())
    if (abc /= 'abc') stop 5
  end associate

  table = dtab_v
  associate (abc => table(1)%rc()//table(2)%rc()//table(3)%rc())
    if (abc /= 'abc') stop 6
  end associate

  table = dtab_p
  associate (abc => table(1)%rc()//table(2)%rc()//table(3)%rc())
    if (abc /= 'abc') stop 7
  end associate

  ! Dispatch table from other module directly used in associate
  associate (abc => dtab_v(1)%rc()//dtab_v(2)%rc()//dtab_v(3)%rc())
    if (abc /= 'abc') stop 8
  end associate

! associate (abc => dtab_p(1)%rc()//dtab_p(2)%rc()//dtab_p(3)%rc())
!   if (abc /= 'abc') stop 9
! end associate

  ! Several variations
  block
    type(pc)            :: table(3) = [pc(a),pc(b),pc(c)]
    associate (abc => table(1)%rc()//table(2)%rc()//table(3)%rc())
      if (abc /= 'abc') stop 10
    end associate
  end block

  block
    use dispatch_table, only: table => dtab_v
    associate (abc => table(1)%rc()//table(2)%rc()//table(3)%rc())
      if (abc /= 'abc') stop 11
    end associate
  end block

! block
!   type(pc), parameter :: table(3) = [pc(a),pc(b),pc(c)]
!   associate (abc => table(1)%rc()//table(2)%rc()//table(3)%rc())
!     if (abc /= 'abc') stop 12
!   end associate
! end block

! block
!   use dispatch_table, only: table => dtab_p
!   associate (abc => table(1)%rc()//table(2)%rc()//table(3)%rc())
!     if (abc /= 'abc') stop 13
!   end associate
! end block

end program
