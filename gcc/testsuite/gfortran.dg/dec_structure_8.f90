! { dg-do compile }
! { dg-options "-fdec-structure -fmax-errors=0" }
!
! Comprehensive compile tests for what structures CAN'T do.
!

! Old-style (clist) initialization
integer,parameter :: as = 3
structure /t1/              ! { dg-error "Type definition.*T1" }
  integer*1 a /300_2/       ! { dg-error "Arithmetic overflow" }
  integer   b //            ! { dg-error "Empty old style initializer list" }
  integer   c /2*3/         ! { dg-error "Repeat spec invalid in scalar" }
  integer   d /1,2,3/       ! { dg-error "End of scalar initializer expected" }
  integer   e /"HI"/        ! { dg-error "Cannot convert" }
  integer   f(as) /4*9/     ! { dg-error "Too many elements" }
  integer   g(3) /1,3/      ! { dg-error "Not enough elements" }
  integer   h(3) /1,3,5,7/  ! { dg-error "Too many elements" }
  integer   i(3) /2*1/      ! { dg-error "Not enough elements" }
  integer   j(3) /10*1/     ! { dg-error "Too many elements" }
  integer   k(3) /2.5*3/    ! { dg-error "Repeat spec must be an integer" }
  integer   l(2) /2*/       ! { dg-error "Expected data constant" }
  integer   m(1) /          ! { dg-error "Syntax error in old style" }
  integer   n(2) /1         ! { dg-error "Syntax error in old style" }
  integer   o(2) /1,        ! { dg-error "Syntax error in old style" }
  integer   p(1) /x/        ! { dg-error "must be a PARAMETER" }
end structure

structure              ! { dg-error "Structure name expected" }
structure /            ! { dg-error "Structure name expected" }
structure //           ! { dg-error "Structure name expected" }
structure /.or./       ! { dg-error "Structure name expected" }
structure /integer/    ! { dg-error "Structure name.*cannot be the same" }
structure /foo/ bar    ! { dg-error "Junk after" }
structure /t1/         ! { dg-error "Type definition.*T1" }

record                 ! { dg-error "Structure name expected" }
record bar             ! { dg-error "Structure name expected" }
record / bar           ! { dg-error "Structure name expected" }
record // bar          ! { dg-error "Structure name expected" }
record foo/ bar        ! { dg-error "Structure name expected" }
record /foo bar        ! { dg-error "Structure name expected" }
record /foo/ bar       ! { dg-error "used before it is defined" }
record /t1/            ! { dg-error "Invalid character in name" }

structure /t2/
  ENTRY here           ! { dg-error "ENTRY statement.*cannot appear" }
  integer a            ! { dg-error "Component.*already declared" }
  integer a            ! { dg-error "Component.*already declared" }
  structure $z         ! { dg-error "Invalid character in name" }
  structure //         ! { dg-error "Invalid character in name" }
  structure // x       ! { dg-error "Invalid character in name" }
  structure /t3/       ! { dg-error "Invalid character in name" }
  structure /t3/ x,$y  ! { dg-error "Invalid character in name" }
  structure /t4/ y     ! { dg-error "Type definition.*T4" }
    integer i, j, k
  end structure
  structure /t4/ z     ! { dg-error "Type definition.*T4" }
end structure

end
