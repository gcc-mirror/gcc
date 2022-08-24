! { dg-do compile }

module declare_target_2
  !$omp declare target to (a) link (a)	! { dg-error "mentioned multiple times in clauses of the same OMP DECLARE TARGET directive" }
  !$omp declare target (b)
  !$omp declare target link (b)		! { dg-error "TO or ENTER clause and later in LINK" }
  !$omp declare target link (f)
  !$omp declare target to (f)		! { dg-error "LINK clause and later in TO" }
  !$omp declare target(c, c)		! { dg-error "mentioned multiple times in clauses of the same" }
  !$omp declare target to (d) to (d)	! { dg-error "mentioned multiple times in clauses of the same" }
  !$omp declare target link (e, e)	! { dg-error "mentioned multiple times in clauses of the same" }
  integer, save :: a, b, c, d, e, f
  interface
    integer function f1 (a)
      !$omp declare target (f1)		! { dg-error "form without clauses is allowed in interface block" }
      integer :: a
    end function
  end interface
  interface
    integer function f2 (a)
      !$omp declare target to (f2)	! { dg-error "form without clauses is allowed in interface block" }
      integer :: a
    end function
  end interface
end
subroutine bar
  !$omp declare target link (baz)	! { dg-error "isn.t SAVEd" }
  call baz				! { dg-error "attribute conflicts" }
end subroutine
subroutine foo				! { dg-error "attribute conflicts" }
  integer :: g, h, i, j, k, l, m, n, o, p, q
  common /c1/ g, h
  common /c2/ i, j
  common /c3/ k, l
  common /c4/ m, n
  common /c5/ o, p, q
  !$omp declare target to (g)		! { dg-error "is an element of a COMMON block" }
  !$omp declare target link (foo)
  !$omp declare target to (/c2/)
  !$omp declare target (/c2/)
  !$omp declare target to(/c2/)
  !$omp declare target link(/c2/)	! { dg-error "TO or ENTER clause and later in LINK" }
  !$omp declare target link(/c3/)
  !$omp declare target (/c3/)		! { dg-error "LINK clause and later in ENTER" }
  !$omp declare target (/c4/, /c4/)	! { dg-error "mentioned multiple times in clauses of the same" }
  !$omp declare target to (/c4/) to(/c4/) ! { dg-error "mentioned multiple times in clauses of the same" }
  !$omp declare target link (/c5/)
  !$omp declare target link (/c5/)
  !$omp declare target link(/c5/)link(/c5/) ! { dg-error "mentioned multiple times in clauses of the same" }
  !$omp declare target link(/c5/,/c5/)	! { dg-error "mentioned multiple times in clauses of the same" }
end subroutine

module declare_target_3
  !$omp declare target enter (a) link (a)	! { dg-error "mentioned multiple times in clauses of the same OMP DECLARE TARGET directive" }
  !$omp declare target link(b) enter(b)	! { dg-error "mentioned multiple times in clauses of the same OMP DECLARE TARGET directive" }
  !$omp declare target to (c) enter (c)	! { dg-error "mentioned multiple times in clauses of the same" }
  !$omp declare target enter (d) to (d)	! { dg-error "mentioned multiple times in clauses of the same" }
  !$omp declare target enter (e) enter (e)	! { dg-error "mentioned multiple times in clauses of the same" }
  integer, save :: a, b, c, d, e
end

