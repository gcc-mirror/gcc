module main
  implicit none
contains
  subroutine f0 ()
  end subroutine
  subroutine f1 ()
  end subroutine
  subroutine f2 ()
    !$omp declare variant	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f3 ()
    !$omp declare variant (	! { dg-error "" }
  end subroutine
  subroutine f4 ()
    !$omp declare variant ()	! { dg-error "" }
  end subroutine
  subroutine f5 ()
    !$omp declare variant match(user={condition(.false.)})	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f6 ()
    !$omp declare variant (f1)	! { dg-error "expected 'match' or 'adjust_args' at .1." }
  end subroutine
  subroutine f7 ()
    !$omp declare variant (f1) simd	! { dg-error "expected 'match' or 'adjust_args' at .1." }
  end subroutine
  subroutine f8 ()
    !$omp declare variant (f1) match	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f9 ()
    !$omp declare variant (f1) match(	! { dg-error "expected context selector set name at .1." }
  end subroutine
  subroutine f10 ()
    !$omp declare variant (f1) match()	! { dg-error "expected context selector set name at .1." }
  end subroutine
  subroutine f11 ()
    !$omp declare variant (f1) match(foo)	! { dg-error "expected context selector set name at .1." }
  end subroutine
  subroutine f12 ()
    !$omp declare variant (f1) match(something={something})	! { dg-error "expected context selector set name at .1." }
  end subroutine
  subroutine f13 ()
    !$omp declare variant (f1) match(user)	! { dg-error "expected '=' at .1." }
  end subroutine
  subroutine f14 ()
    !$omp declare variant (f1) match(user=)	! { dg-error "expected '\\\{' at .1." }
  end subroutine
  subroutine f15 ()
    !$omp declare variant (f1) match(user=	! { dg-error "expected '\\\{' at .1." }
  end subroutine
  subroutine f16 ()
    !$omp declare variant (f1) match(user={)	! { dg-error "expected trait selector name at .1." }
  end subroutine
  subroutine f17 ()
    !$omp declare variant (f1) match(user={})	! { dg-error "expected trait selector name at .1." }
  end subroutine
  subroutine f18 ()
    !$omp declare variant (f1) match(user={condition})	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f19 ()
    !$omp declare variant (f1) match(user={condition(})	! { dg-error "expected expression at .1." }
  end subroutine
  subroutine f20 ()
    !$omp declare variant (f1) match(user={condition()})	! { dg-error "expected expression at .1." }
  end subroutine
  subroutine f21 ()
    !$omp declare variant (f1) match(user={condition(f1)})	! { dg-error "expected expression at .1." }
  end subroutine
  subroutine f22 ()
    !$omp declare variant (f1) match(user={condition(.false., .true., .false.)})	! { dg-error "expected '\\)' at .1." }
  end subroutine
  subroutine f23 ()
    !$omp declare variant (f1) match(construct={master})	! { dg-warning "unknown selector 'master' for context selector set 'construct'" }
  end subroutine
  subroutine f24 ()
    !$omp declare variant (f1) match(construct={teams,parallel,master,do})	! { dg-warning "unknown selector 'master' for context selector set 'construct'" }
  end subroutine
  subroutine f25 ()
    !$omp declare variant (f1) match(construct={parallel(1	! { dg-error "selector 'parallel' does not accept any properties at .1." }
  end subroutine
  subroutine f26 ()
    !$omp declare variant (f1) match(construct={parallel(1)})	! { dg-error "selector 'parallel' does not accept any properties at .1." }
  end subroutine
  subroutine f27 ()
    !$omp declare variant (f0) match(construct={simd(12)})	! { dg-error "expected simd clause at .1." }
  end subroutine
  subroutine f32 ()
    !$omp declare variant (f1) match(device={kind})	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f33 ()
    !$omp declare variant (f1) match(device={isa})	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f34 ()
    !$omp declare variant (f1) match(device={arch})	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f35 ()
    !$omp declare variant (f1) match(device={kind,isa,arch})	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f36 ()
    !$omp declare variant (f1) match(device={kind(})	! { dg-error "expected identifier or string literal at .1." }
  end subroutine
  subroutine f39 ()
    !$omp declare variant (f1) match(device={isa(1)})	! { dg-error "expected identifier or string literal at .1." }
  end subroutine
  subroutine f40 ()
    !$omp declare variant (f1) match(device={arch(17)})	! { dg-error "expected identifier or string literal at .1." }
  end subroutine
  subroutine f41 ()
    !$omp declare variant (f1) match(device={foobar(3)}) ! { dg-warning "unknown selector 'foobar' for context selector set 'device' at .1." }
  end subroutine
  subroutine f43 ()
    !$omp declare variant (f1) match(implementation={foobar(3)}) ! { dg-warning "unknown selector 'foobar' for context selector set 'implementation' at .1." }
  end subroutine
  subroutine f44 ()
    !$omp declare variant (f1) match(implementation={vendor})	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f45 ()
    !$omp declare variant (f1) match(implementation={extension})	! { dg-error "expected '\\(' at .1." }
  end subroutine
  subroutine f45a ()
    !$omp declare variant (f1) match(implementation={vendor()})	! { dg-error "expected identifier or string literal at .1." }
  end subroutine
  subroutine f46 ()
    !$omp declare variant (f1) match(implementation={vendor(123-234)})	! { dg-error "expected identifier or string literal at .1." }
  end subroutine
  subroutine f48 ()
    !$omp declare variant (f1) match(implementation={unified_address(yes)})	! { dg-error "selector 'unified_address' does not accept any properties at .1." }
  end subroutine
  subroutine f49 ()
    !$omp declare variant (f1) match(implementation={unified_shared_memory(no)})	! { dg-error "selector 'unified_shared_memory' does not accept any properties at .1." }
  end subroutine
  subroutine f50 ()
    !$omp declare variant (f1) match(implementation={dynamic_allocators(42)})	! { dg-error "selector 'dynamic_allocators' does not accept any properties at .1." }
  end subroutine
  subroutine f51 ()
    !$omp declare variant (f1) match(implementation={reverse_offload()})	! { dg-error "selector 'reverse_offload' does not accept any properties at .1." }
  end subroutine
  subroutine f52 ()
    !$omp declare variant (f1) match(implementation={atomic_default_mem_order})	! { dg-error "expected '\\('" }
  end subroutine
  subroutine f56 ()
    !$omp declare variant (f1) match(implementation={atomic_default_mem_order(relaxed,seq_cst)})	! { dg-error "expected '\\)' at .1." }
  end subroutine
  subroutine f58 ()
    !$omp declare variant (f1) match(user={foobar(3)})	! { dg-warning "unknown selector 'foobar' for context selector set 'user' at .1." }
  end subroutine
  subroutine f59 ()
    !$omp declare variant (f1) match(construct={foobar(3)})	! { dg-warning "unknown selector 'foobar' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f60 ()
    !$omp declare variant (f1) match(construct={parallel},foobar={bar})	! { dg-error "expected context selector set name at .1." }
  end subroutine
  subroutine f64 ()
    !$omp declare variant (f1) match(construct={single})	! { dg-warning "unknown selector 'single' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f65 ()
    !$omp declare variant (f1) match(construct={taskgroup})	! { dg-warning "unknown selector 'taskgroup' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f66 ()
    !$omp declare variant (f1) match(construct={for})	! { dg-warning "unknown selector 'for' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f67 ()
    !$omp declare variant (f1) match(construct={threadprivate})	! { dg-warning "unknown selector 'threadprivate' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f68 ()
    !$omp declare variant (f1) match(construct={critical})	! { dg-warning "unknown selector 'critical' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f69 ()
    !$omp declare variant (f1) match(construct={task})	! { dg-warning "unknown selector 'task' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f70 ()
    !$omp declare variant (f1) match(construct={taskloop})	! { dg-warning "unknown selector 'taskloop' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f71 ()
    !$omp declare variant (f1) match(construct={sections})	! { dg-warning "unknown selector 'sections' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f72 ()
    !$omp declare variant (f1) match(construct={section})	! { dg-warning "unknown selector 'section' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f73 ()
    !$omp declare variant (f1) match(construct={workshare})	! { dg-warning "unknown selector 'workshare' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f74 ()
    !$omp declare variant (f1) match(construct={requires})	! { dg-warning "unknown selector 'requires' for context selector set 'construct' at .1." }
  end subroutine
  subroutine f75a ()
    !$omp declare variant(f1) ,,match(construct={dispatch}) adjust_args(need_device_ptr : c)        ! { dg-error "expected 'match' or 'adjust_args' at .1." }
  end subroutine
  subroutine f75b ()
    !$omp declare variant(f1) match(construct={dispatch}),,adjust_args(need_device_ptr : c)        ! { dg-error "expected 'match' or 'adjust_args' at .1." }
  end subroutine
  subroutine f75c ()
    !$omp declare variant(f1) match(construct={dispatch}),nowait(a)        ! { dg-error "expected 'match' or 'adjust_args' at .1." }
  end subroutine
  subroutine f76 ()
    !$omp declare variant (f1) match(implementation={atomic_default_mem_order("relaxed")})	! { dg-error "expected identifier at .1." }
  end subroutine
  subroutine f77 ()
    !$omp declare variant (f1) match(user={condition(score(f76):.true.)})  ! { dg-error ".score. argument must be constant integer expression at .1." }
  end subroutine
  subroutine f78 ()
    !$omp declare variant (f1) match(user={condition(score(-130):.true.)}) ! { dg-error ".score. argument must be non-negative" }
  end subroutine
end module
