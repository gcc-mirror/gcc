! { dg-do run }
! { dg-options "-fno-automatic -finit-local-zero -fdump-tree-original" }
!
! PR fortran/62309
!
! Make sure variables are saved with -fno-automatic except in
! functions marked RECURSIVE, and that they are still initialized with
! -finit-local-zero.
!

function f (x)
implicit none
  integer f, x
  integer a ! should be SAVEd
  a = a + x ! should increment by y every time
  f = a
  return
endfunction

function f2 (x)
implicit none
  integer f2, x
  block
   named: block
    block
    integer a ! should be SAVEd
    a = a + x ! should increment by y every time
    f2 = a
    end block
   end block named
  end block
  return
endfunction

recursive function g (x)
implicit none
  integer g, x
  integer b ! should be automatic
  b = b + x ! should be set to y every time
  g = b
  return
endfunction

recursive function g2 (x)
implicit none
  integer g2, x
  block
   named: block
    block
    integer b ! should be automatic
    b = b + x ! should be set to y every time
    g2 = b
    end block
   end block named
  end block
  return
endfunction

implicit none
integer f, f2, g, g2

! Should return static value of a; accumulates y
if ( f(3) .ne. 3 ) call abort ()
if ( f(4) .ne. 7 ) call abort ()
if ( f(2) .ne. 9 ) call abort ()

if ( f2(3) .ne. 3 ) call abort ()
if ( f2(4) .ne. 7 ) call abort ()
if ( f2(2) .ne. 9 ) call abort ()

! Should return automatic value of a; equal to y each time
if ( g(3) .ne. 3 ) call abort ()
if ( g(4) .ne. 4 ) call abort ()
if ( g(2) .ne. 2 ) call abort ()

if ( g2(3) .ne. 3 ) call abort ()
if ( g2(4) .ne. 4 ) call abort ()
if ( g2(2) .ne. 2 ) call abort ()

end

! { dg-final { scan-tree-dump-times "  static integer\\\(kind=4\\\) a = 0;" 2 "original" } }
! { dg-final { scan-tree-dump-times "  b = 0;" 2 "original" } }
