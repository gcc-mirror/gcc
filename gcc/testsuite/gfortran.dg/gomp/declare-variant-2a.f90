module main
  implicit none
contains
  subroutine f1 ()
  end subroutine
  subroutine f28 ()
    !$omp declare variant (f1) match(construct={parallel},construct={do})  ! { dg-error "selector set 'construct' specified more than once" }
  end subroutine
  subroutine f29 ()
    !$omp declare variant (f1) match(construct={parallel},construct={parallel}) ! { dg-error "selector set 'construct' specified more than once" }
  end subroutine
  subroutine f30 ()
    !$omp declare variant (f1) match(user={condition(.false.)},construct={target},user={condition(.false.)})  ! { dg-error "selector set 'user' specified more than once" }
  end subroutine
  subroutine f31 ()
    !$omp declare variant (f1) match(user={condition(.false.)},user={condition(.true.)}) ! { dg-error "selector set 'user' specified more than once" }
  end subroutine
  subroutine f37 ()
    !$omp declare variant (f1) match(device={kind(unknown)})  ! { dg-warning "unknown property 'unknown' of 'kind' selector" }
  end subroutine
  subroutine f38 ()
    !$omp declare variant (f1) match(device={kind(unknown,foobar)})	! { dg-warning "unknown property 'unknown' of 'kind' selector" }
									! { dg-warning "unknown property 'foobar' of 'kind' selector" "" { target *-*-* } 22 }
  end subroutine
  subroutine f42 ()
    !$omp declare variant (f1) match(device={arch(x86_64)},device={isa(avx512vl)})  ! { dg-error "selector set 'device' specified more than once" }
  end subroutine
  subroutine f47 ()
    !$omp declare variant (f1) match(implementation={vendor("foobar")}) ! { dg-warning "unknown property '.foobar.' of 'vendor' selector" }
  end subroutine
  subroutine f53 ()
    !$omp declare variant (f1) match(implementation={atomic_default_mem_order(acquire)})
  end subroutine
  subroutine f54 ()
    !$omp declare variant (f1) match(implementation={atomic_default_mem_order(release)})
  end subroutine
  subroutine f55 ()
    !$omp declare variant (f1) match(implementation={atomic_default_mem_order(foobar)}) ! { dg-error "incorrect property 'foobar' of 'atomic_default_mem_order' selector" }
  end subroutine
  subroutine f57 ()
    !$omp declare variant (f1) match(implementation={atomic_default_mem_order(relaxed)},&
    !$omp & implementation={atomic_default_mem_order(relaxed)}) ! { dg-error "selector set 'implementation' specified more than once" "" { target *-*-* } 41  }
  end subroutine
  subroutine f61 ()
    !$omp declare variant (f1) match(construct={parallel,parallel}) ! { dg-error "selector 'parallel' specified more than once in set 'construct'" }
  end subroutine
  subroutine f62 ()
    !$omp declare variant (f1) match(construct={target,parallel,do,simd,parallel}) ! { dg-error "selector 'parallel' specified more than once in set 'construct'" }
  end subroutine
  subroutine f63 ()
    !$omp declare variant (f1) match(construct={target,teams,teams})  ! { dg-error "selector 'teams' specified more than once in set 'construct'" }
  end subroutine
end module
