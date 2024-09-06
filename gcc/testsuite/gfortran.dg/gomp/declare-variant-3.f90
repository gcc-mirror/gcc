module main
contains
  subroutine f1 ()
  end subroutine
  subroutine f2 ()
    !$omp declare variant (f1) match (construct={target})
  end subroutine
  subroutine f3 ()
  end subroutine
  subroutine f4 ()
    !$omp declare variant (f3) match (construct={teams})
  end subroutine
  subroutine f5 ()
  end subroutine
  subroutine f6 ()
    !$omp declare variant (f5) match (construct={parallel})
  end subroutine
  subroutine f7 ()
  end subroutine
  subroutine f8 ()
    !$omp declare variant (f7) match (construct={do})
  end subroutine
  subroutine f9 ()
  end subroutine
  subroutine f10 ()
    !$omp declare variant (f9) match (construct={target,teams,parallel,do})
  end subroutine
  subroutine f11 ()
  end subroutine
  subroutine f12 ()
    !$omp declare variant (f11) match (construct={teams,do,parallel})
  end subroutine
  subroutine f13 ()
  end subroutine
  subroutine f14 ()
    !$omp declare variant (f13) match (device={kind(any)})
  end subroutine
  subroutine f15 ()
    !$omp declare variant (f13) match (device={kind("host")})
  end subroutine
  subroutine f16 ()
    !$omp declare variant (f13) match (device={kind(nohost)})
  end subroutine
  subroutine f17 ()
    !$omp declare variant (f13) match (device={kind(cpu)})
  end subroutine
  subroutine f18 ()
    !$omp declare variant (f13) match (device={kind("gpu")})
  end subroutine
  subroutine f19 ()
    !$omp declare variant (f13) match (device={kind(fpga)})
  end subroutine
  subroutine f20 ()
    !$omp declare variant (f13) match (device={kind(any)})
  end subroutine
  subroutine f21 ()
    !$omp declare variant (f13) match (device={kind(host,nohost)})
  end subroutine
  subroutine f22 ()
    !$omp declare variant (f13) match (device={kind("cpu","gpu","fpga")})
  end subroutine
  subroutine f23 ()
    !$omp declare variant (f13) match (device={kind(cpu,nohost)})
  end subroutine
  subroutine f24 ()
    !$omp declare variant (f13) match (device={isa(avx)})
  end subroutine
  subroutine f25 ()
    !$omp declare variant (f13) match (device={isa(sse4,"avx512f",avx512vl,avx512bw)})
  end subroutine
  subroutine f26 ()
    !$omp declare variant (f13) match (device={arch("x86_64")})
  end subroutine
  subroutine f27 ()
    !$omp declare variant (f13) match (device={arch(riscv64)})
  end subroutine
  subroutine f28 ()
    !$omp declare variant (f13) match (device={arch(nvptx)})
  end subroutine
  subroutine f29 ()
    !$omp declare variant (f13) match (device={arch(x86_64),isa("avx512f","avx512vl"),kind(cpu)})
  end subroutine
  subroutine f30 ()
    !$omp declare variant (f13) match (implementation={vendor(amd)})
  end subroutine
  subroutine f31 ()
    !$omp declare variant (f13) match (implementation={vendor(arm)})
  end subroutine
  subroutine f32 ()
    !$omp declare variant (f13) match (implementation={vendor("bsc")})
  end subroutine
  subroutine f33 ()
    !$omp declare variant (f13) match (implementation={vendor(cray)})
  end subroutine
  subroutine f34 ()
    !$omp declare variant (f13) match (implementation={vendor(fujitsu)})
  end subroutine
  subroutine f35 ()
    !$omp declare variant (f13) match (implementation={vendor(gnu)})
  end subroutine
  subroutine f36 ()
    !$omp declare variant (f13) match (implementation={vendor(ibm)})
  end subroutine
  subroutine f37 ()
    !$omp declare variant (f13) match (implementation={vendor("intel")})
  end subroutine
  subroutine f38 ()
    !$omp declare variant (f13) match (implementation={vendor(llvm)})
  end subroutine
  subroutine f39 ()
    !$omp declare variant (f13) match (implementation={vendor(pgi)})
  end subroutine
  subroutine f40 ()
    !$omp declare variant (f13) match (implementation={vendor(ti)})
  end subroutine
  subroutine f41 ()
    !$omp declare variant (f13) match (implementation={vendor(unknown)})
  end subroutine
  subroutine f42 ()
    !$omp declare variant (f13) match (implementation={vendor(gnu,llvm,intel,ibm)})
  end subroutine
  subroutine f43 ()
    !$omp declare variant (f13) match (implementation={extension(my_cute_extension)})	! { dg-warning "unknown property 'my_cute_extension' of 'extension' selector" }
  end subroutine
  subroutine f44 ()
    !$omp declare variant (f13) match (implementation={extension(some_other_ext,another_ext)})	! { dg-warning "unknown property 'some_other_ext' of 'extension' selector" }
												! { dg-warning "unknown property 'another_ext' of 'extension' selector" "" { target *-*-* } .-1 }
  end subroutine
  subroutine f45 ()
    !$omp declare variant (f13) match (implementation={unified_shared_memory})
  end subroutine
  subroutine f45a ()
    !$omp declare variant (f13) match (implementation={self_maps})
  end subroutine
  subroutine f46 ()
    !$omp declare variant (f13) match (implementation={unified_address})
  end subroutine
  subroutine f47 ()
    !$omp declare variant (f13) match (implementation={dynamic_allocators})
  end subroutine
  subroutine f48 ()
    !$omp declare variant (f13) match (implementation={reverse_offload})
  end subroutine
  subroutine f49 ()
    !$omp declare variant (f13) match (implementation={atomic_default_mem_order(seq_cst)})
  end subroutine
  subroutine f50 ()
    !$omp declare variant (f13) match (implementation={atomic_default_mem_order(relaxed)})
  end subroutine
  subroutine f51 ()
    !$omp declare variant (f13) match (implementation={atomic_default_mem_order(acq_rel)})
  end subroutine
  subroutine f52 ()
    !$omp declare variant (f14) match (implementation={atomic_default_mem_order(acq_rel),vendor(gnu),&
    !$omp&					       unified_address,extension(foobar)}) ! { dg-warning "unknown property 'foobar' of 'extension' selector" "" { target *-*-* } .-1 }
  end subroutine
  subroutine f53 ()
    !$omp declare variant (f13) match (implementation={vendor(score(3):amd)})
  end subroutine
  subroutine f54 ()
    !$omp declare variant (f13) match (implementation={vendor(score(4):"arm")})
  end subroutine
  subroutine f55 ()
    !$omp declare variant (f13) match (implementation={vendor(score(5):bsc)})
  end subroutine
  subroutine f56 ()
    !$omp declare variant (f13) match (implementation={vendor(score(6):cray)})
  end subroutine
  subroutine f57 ()
    !$omp declare variant (f13) match (implementation={vendor(score(7):fujitsu)})
  end subroutine
  subroutine f58 ()
    !$omp declare variant (f13) match (implementation={vendor(score(8):gnu)})
  end subroutine
  subroutine f59 ()
    !$omp declare variant (f13) match (implementation={vendor(score(9):ibm)})
  end subroutine
  subroutine f60 ()
    !$omp declare variant (f13) match (implementation={vendor(score(10):intel)})
  end subroutine
  subroutine f61 ()
    !$omp declare variant (f13) match (implementation={vendor(score(11):llvm)})
  end subroutine
  subroutine f62 ()
    !$omp declare variant (f13) match (implementation={vendor(score(12):pgi)})
  end subroutine
  subroutine f63 ()
    !$omp declare variant (f13) match (implementation={vendor(score(13):"ti")})
  end subroutine
  subroutine f64 ()
    !$omp declare variant (f13) match (implementation={vendor(score(14):unknown)})
  end subroutine
  subroutine f65 ()
    !$omp declare variant (f13) match (implementation={vendor(score(15):gnu,llvm,intel,ibm)})
  end subroutine
  subroutine f66 ()
    !$omp declare variant (f13) match (implementation={extension(score(16):my_cute_extension)})	! { dg-warning "unknown property 'my_cute_extension' of 'extension' selector" }
  end subroutine
  subroutine f67 ()
    !$omp declare variant (f13) match (implementation={extension(score(17):some_other_ext,another_ext)})	! { dg-warning "unknown property 'some_other_ext' of 'extension' selector" }
  end subroutine												! { dg-warning "unknown property 'another_ext' of 'extension' selector" "" { target *-*-* } .-1 }
  subroutine f68 ()
    !$omp declare variant (f13) match (implementation={atomic_default_mem_order(score(18):seq_cst)})
  end subroutine
  subroutine f69 ()
    !$omp declare variant (f13) match (implementation={atomic_default_mem_order(score(19):relaxed)})
  end subroutine
  subroutine f70 ()
    !$omp declare variant (f13) match (implementation={atomic_default_mem_order(score(20):acq_rel)})
  end subroutine
  subroutine f71 ()
    !$omp declare variant (f13) match (implementation={atomic_default_mem_order(score(21):acq_rel),&
    !$omp&					       vendor(score(22):gnu),unified_address,extension(score(22):foobar)})	! { dg-warning "unknown property 'foobar' of 'extension' selector" "" { target *-*-* } .-1 }
  end subroutine
  subroutine f72 ()
    !$omp declare variant (f13) match (user={condition(.false.)})
  end subroutine
  subroutine f73 ()
    !$omp declare variant (f13) match (user={condition(.true..and..not..true.)})
  end subroutine
  subroutine f74 ()
    !$omp declare variant (f13) match (user={condition(score(25):.true.)})
  end subroutine
  subroutine f75 ()
    !$omp declare variant (f13) match (device={kind("any")})
  end subroutine
  subroutine f78 ()
    !$omp declare variant (f13) match (implementation={vendor(nvidia)})
  end subroutine
  subroutine f79 ()
    !$omp declare variant (f13) match (user={condition(score(0):.false.)})
  end subroutine

  end module
