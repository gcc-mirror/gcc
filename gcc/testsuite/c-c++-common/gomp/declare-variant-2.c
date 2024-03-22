void f0 (void);
void f1 (void);
#pragma omp declare variant	/* { dg-error "expected '\\(' before end of line" } */
void f2 (void);
#pragma omp declare variant (	/* { dg-error "" } */
void f3 (void);
#pragma omp declare variant ()	/* { dg-error "" } */
void f4 (void);
#pragma omp declare variant match(user={condition(0)})	/* { dg-error "expected '\\(' before 'match'" } */
void f5 (void);
#pragma omp declare variant (f1)	/* { dg-error "expected 'match' before end of line" } */
void f6 (void);
#pragma omp declare variant (f1) simd	/* { dg-error "expected 'match' before 'simd'" } */
void f7 (void);
#pragma omp declare variant (f1) match	/* { dg-error "expected '\\(' before end of line" } */
void f8 (void);
#pragma omp declare variant (f1) match(	/* { dg-error "expected context selector set name before end of line" } */
void f9 (void);
#pragma omp declare variant (f1) match()	/* { dg-error "expected context selector set name before '\\)' token" } */
void f10 (void);
#pragma omp declare variant (f1) match(foo)	/* { dg-error "expected context selector set name before 'foo'" } */
void f11 (void);
#pragma omp declare variant (f1) match(something={something})	/* { dg-error "expected context selector set name before 'something'" } */
void f12 (void);
#pragma omp declare variant (f1) match(user)	/* { dg-error "expected '=' before '\\)' token" } */
void f13 (void);
#pragma omp declare variant (f1) match(user=)	/* { dg-error "expected '\\\{' before '\\)' token" } */
void f14 (void);
#pragma omp declare variant (f1) match(user=	/* { dg-error "expected '\\\{' before end of line" } */
void f15 (void);
#pragma omp declare variant (f1) match(user={)	/* { dg-error "expected trait selector name before '\\)' token" } */
void f16 (void);				/* { dg-error "expected '\\\}' before" "" { target c++ } .-1 } */
#pragma omp declare variant (f1) match(user={})	/* { dg-error "expected trait selector name before '\\\}' token" } */
void f17 (void);
#pragma omp declare variant (f1) match(user={condition})	/* { dg-error "expected '\\(' before '\\\}' token" } */
void f18 (void);
#pragma omp declare variant (f1) match(user={condition(})	/* { dg-error "expected \[^\n\r]*expression before '\\\}' token" } */
void f19 (void);
#pragma omp declare variant (f1) match(user={condition()})	/* { dg-error "expected \[^\n\r]*expression before '\\)' token" } */
void f20 (void);
#pragma omp declare variant (f1) match(user={condition(f1)})	/* { dg-error "property must be constant integer expression" "" { target { c || c++11 } } } */
void f21 (void);						/* { dg-error "cannot appear in a constant-expression" "" { target c++98_only } .-1 } */
#pragma omp declare variant (f1) match(user={condition(1, 2, 3)})	/* { dg-error "expected '\\)' before ',' token" } */
void f22 (void);
#pragma omp declare variant (f1) match(construct={master})	/* { dg-warning "unknown selector 'master' for context selector set 'construct'" } */
void f23 (void);
#pragma omp declare variant (f1) match(construct={teams,parallel,master,for})	/* { dg-warning "unknown selector 'master' for context selector set 'construct'" } */
void f24 (void);
#pragma omp declare variant (f1) match(construct={parallel(1	/* { dg-error "selector 'parallel' does not accept any properties" } */
void f25 (void);						/* { dg-error "expected '\\\}' before end of line" "" { target c++ } .-1 } */
								/* { dg-error "expected '\\\}' before '\\(' token" "" { target c } .-2 } */
#pragma omp declare variant (f1) match(construct={parallel(1)})	/* { dg-error "selector 'parallel' does not accept any properties" } */
void f26 (void);							/* { dg-error "expected '\\\}' before '\\(' token" "" { target c } .-1 } */
#pragma omp declare variant (f0) match(construct={simd(12)})	/* { dg-error "expected \[^\n\r]* clause before" } */
void f27 (void);						/* { dg-error "'\\)' before numeric constant" "" { target c++ } .-1 } */
#pragma omp declare variant (f1) match(construct={parallel},construct={for})	/* { dg-error "selector set 'construct' specified more than once" } */
void f28 (void);
#pragma omp declare variant (f1) match(construct={parallel},construct={parallel})	/* { dg-error "selector set 'construct' specified more than once" } */
void f29 (void);
#pragma omp declare variant (f1) match(user={condition(0)},construct={target},user={condition(0)})	/* { dg-error "selector set 'user' specified more than once" } */
void f30 (void);
#pragma omp declare variant (f1) match(user={condition(0)},user={condition(1)})	/* { dg-error "selector set 'user' specified more than once" } */
void f31 (void);
#pragma omp declare variant (f1) match(device={kind})	/* { dg-error "expected '\\(' before '\\\}' token" } */
void f32 (void);
#pragma omp declare variant (f1) match(device={isa})	/* { dg-error "expected '\\(' before '\\\}' token" } */
void f33 (void);
#pragma omp declare variant (f1) match(device={arch})	/* { dg-error "expected '\\(' before '\\\}' token" } */
void f34 (void);
#pragma omp declare variant (f1) match(device={kind,isa,arch})	/* { dg-error "expected '\\(' before ',' token" } */
void f35 (void);
#pragma omp declare variant (f1) match(device={kind(})	/* { dg-error "expected identifier or string literal before '\\\}' token" } */
void f36 (void);
#pragma omp declare variant (f1) match(device={kind(unknown)})	/* { dg-warning "unknown property 'unknown' of 'kind' selector" } */
void f37 (void);
#pragma omp declare variant (f1) match(device={kind(unknown,foobar)})	/* { dg-warning "unknown property 'unknown' of 'kind' selector" } */
void f38 (void);							/* { dg-warning "unknown property 'foobar' of 'kind' selector" "" { target *-*-* } .-1 } */
#pragma omp declare variant (f1) match(device={isa(1)})	/* { dg-error "expected identifier or string literal before numeric constant" } */
void f39 (void);
#pragma omp declare variant (f1) match(device={arch(17)})	/* { dg-error "expected identifier or string literal before numeric constant" } */
void f40 (void);
#pragma omp declare variant (f1) match(device={foobar(3)})	/* { dg-warning "unknown selector 'foobar' for context selector set 'device'" } */
void f41 (void);
#pragma omp declare variant (f1) match(device={arch(x86_64)},device={isa(avx512vl)})	/* { dg-error "selector set 'device' specified more than once" } */
void f42 (void);
#pragma omp declare variant (f1) match(implementation={foobar(3)})	/* { dg-warning "unknown selector 'foobar' for context selector set 'implementation'" } */
void f43 (void);
#pragma omp declare variant (f1) match(implementation={vendor})	/* { dg-error "expected '\\(' before '\\\}' token" } */
void f44 (void);
#pragma omp declare variant (f1) match(implementation={extension})	/* { dg-error "expected '\\(' before '\\\}' token" } */
void f45 (void);
#pragma omp declare variant (f1) match(implementation={vendor()})	/* { dg-error "expected identifier or string literal before '\\)' token" } */
void f45 (void);
#pragma omp declare variant (f1) match(implementation={vendor(123-234)})	/* { dg-error "expected identifier or string literal before numeric constant" } */
void f46 (void);
#pragma omp declare variant (f1) match(implementation={vendor("foobar")})	/* { dg-warning "unknown property '.foobar.' of 'vendor' selector" } */
void f47 (void);
#pragma omp declare variant (f1) match(implementation={unified_address(yes)})	/* { dg-error "selector 'unified_address' does not accept any properties" } */
void f48 (void);								/* { dg-error "expected '\\\}' before '\\(' token" "" { target c } .-1 } */
#pragma omp declare variant (f1) match(implementation={unified_shared_memory(no)})	/* { dg-error "selector 'unified_shared_memory' does not accept any properties" } */
void f49 (void);									/* { dg-error "expected '\\\}' before '\\(' token" "" { target c } .-1 } */
#pragma omp declare variant (f1) match(implementation={dynamic_allocators(42)})	/* { dg-error "selector 'dynamic_allocators' does not accept any properties" } */
void f50 (void);								/* { dg-error "expected '\\\}' before '\\(' token" "" { target c } .-1 } */
#pragma omp declare variant (f1) match(implementation={reverse_offload()})	/* { dg-error "selector 'reverse_offload' does not accept any properties" } */
void f51 (void);								/* { dg-error "expected '\\\}' before '\\(' token" "" { target c } .-1 } */
#pragma omp declare variant (f1) match(implementation={atomic_default_mem_order})	/* { dg-error "expected '\\(' before '\\\}' token" } */
void f52 (void);
#pragma omp declare variant (f1) match(implementation={atomic_default_mem_order(acquire)})
void f53 (void);
#pragma omp declare variant (f1) match(implementation={atomic_default_mem_order(release)})
void f54 (void);
#pragma omp declare variant (f1) match(implementation={atomic_default_mem_order(foobar)})	/* { dg-error "incorrect property 'foobar' of 'atomic_default_mem_order' selector" } */
void f55 (void);
#pragma omp declare variant (f1) match(implementation={atomic_default_mem_order(relaxed,seq_cst)})	/* { dg-error "expected '\\)' before ',' token" } */
void f56 (void);
#pragma omp declare variant (f1) match(implementation={atomic_default_mem_order(relaxed)},implementation={atomic_default_mem_order(relaxed)})	/* { dg-error "selector set 'implementation' specified more than once" } */
void f57 (void);
#pragma omp declare variant (f1) match(user={foobar(3)})	/* { dg-warning "unknown selector 'foobar' for context selector set 'user'" } */
void f58 (void);
#pragma omp declare variant (f1) match(construct={foobar(3)})	/* { dg-warning "unknown selector 'foobar' for context selector set 'construct'" } */
void f59 (void);
#pragma omp declare variant (f1) match(construct={parallel},foobar={bar})	/* { dg-error "expected context selector set name before 'foobar'" } */
void f60 (void);
#pragma omp declare variant (f1) match(construct={parallel,parallel})	/* { dg-error "selector 'parallel' specified more than once in set 'construct'" } */
void f61 (void);
#pragma omp declare variant (f1) match(construct={target,parallel,for,simd,parallel})	/* { dg-error "selector 'parallel' specified more than once in set 'construct'" } */
void f62 (void);
#pragma omp declare variant (f1) match(construct={target,teams,teams})	/* { dg-error "selector 'teams' specified more than once in set 'construct'" } */
void f63 (void);
#pragma omp declare variant (f1) match(construct={single})	/* { dg-warning "unknown selector 'single' for context selector set 'construct'" } */
void f64 (void);
#pragma omp declare variant (f1) match(construct={taskgroup})	/* { dg-warning "unknown selector 'taskgroup' for context selector set 'construct'" } */
void f65 (void);
#pragma omp declare variant (f1) match(construct={do})	/* { dg-warning "unknown selector 'do' for context selector set 'construct'" } */
void f66 (void);
#pragma omp declare variant (f1) match(construct={threadprivate})	/* { dg-warning "unknown selector 'threadprivate' for context selector set 'construct'" } */
void f67 (void);
#pragma omp declare variant (f1) match(construct={critical})	/* { dg-warning "unknown selector 'critical' for context selector set 'construct'" } */
void f68 (void);
#pragma omp declare variant (f1) match(construct={task})	/* { dg-warning "unknown selector 'task' for context selector set 'construct'" } */
void f69 (void);
#pragma omp declare variant (f1) match(construct={taskloop})	/* { dg-warning "unknown selector 'taskloop' for context selector set 'construct'" } */
void f70 (void);
#pragma omp declare variant (f1) match(construct={sections})	/* { dg-warning "unknown selector 'sections' for context selector set 'construct'" } */
void f71 (void);
#pragma omp declare variant (f1) match(construct={section})	/* { dg-warning "unknown selector 'section' for context selector set 'construct'" } */
void f72 (void);
#pragma omp declare variant (f1) match(construct={workshare})	/* { dg-warning "unknown selector 'workshare' for context selector set 'construct'" } */
void f73 (void);
#pragma omp declare variant (f1) match(construct={requires})	/* { dg-warning "unknown selector 'requires' for context selector set 'construct'" } */
void f74 (void);
void f75a (void);
#pragma omp declare variant (f75a),match(construct={parallel})
void f75 (void);
#pragma omp declare variant (f1) match(implementation={atomic_default_mem_order("relaxed")})	/* { dg-error "expected identifier before string constant" } */
void f76 (void);
#pragma omp declare variant (f1) match(user={condition(score(&f76):1)})	/* { dg-error ".score. argument must be constant integer expression" "" { target { ! c++98_only } } } */
void f77 (void);							/* { dg-error "cannot appear in a constant-expression" "" { target c++98_only } .-1 } */
#pragma omp declare variant (f1) match(user={condition(score(-130):1)})	/* { dg-error ".score. argument must be non-negative" } */
void f78 (void);
