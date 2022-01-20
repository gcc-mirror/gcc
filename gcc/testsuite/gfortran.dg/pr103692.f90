! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! PR fortran/103692 - ICE in expand_constructor
! Contributed by G.Steinmetz

program p
  character(3), parameter :: a(4) = 'abc'
  character(*), parameter :: b(*) =  (a(2:1))
  character(*), parameter :: y(*) = [(a(2:1))]
  character(*), parameter :: u(*) =  a(2:1)
  character(*), parameter :: v(*) = [a(2:1)]
  character(*), parameter :: w(-1) = (a(2:1))
  character(*), parameter :: x(-1) =  a(2:1)
  character(5), parameter :: c(3,3) = 'def'
  character(*), parameter :: d(*)   = [(c(2:1,2:))]
  character(*), parameter :: e(*,*) =  (c(2:1,2:))
  if (len(b) /= 3 .or. size (b) /= 0) stop 1
  if (len(y) /= 3 .or. size (y) /= 0) stop 2
  if (len(d) /= 5 .or. size (d) /= 0) stop 3
  if (len(e) /= 5 .or. any (shape (e) /= [0,2])) stop 4
end

! { dg-final { scan-tree-dump-not "_gfortran_stop_numeric" "original" } }
