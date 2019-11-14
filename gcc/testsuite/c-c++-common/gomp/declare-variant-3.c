void f1 (void);
#pragma omp declare variant (f1) match (construct={target})
void f2 (void);
void f3 (void);
#pragma omp declare variant (f3) match (construct={teams})
void f4 (void);
void f5 (void);
#pragma omp declare variant (f5) match (construct={parallel})
void f6 (void);
void f7 (void);
#pragma omp declare variant (f7) match (construct={for})
void f8 (void);
void f9 (void);
#pragma omp declare variant (f9) match (construct={target,teams,parallel,for})
void f10 (void);
void f11 (void);
#pragma omp declare variant (f11) match (construct={teams,for,parallel})
void f12 (void);
void f13 (void);
#pragma omp declare variant (f13) match (device={kind(any)})
void f14 (void);
#pragma omp declare variant (f13) match (device={kind("host")})
void f15 (void);
#pragma omp declare variant (f13) match (device={kind(nohost)})
void f16 (void);
#pragma omp declare variant (f13) match (device={kind(cpu)})
void f17 (void);
#pragma omp declare variant (f13) match (device={kind("gpu")})
void f18 (void);
#pragma omp declare variant (f13) match (device={kind(fpga)})
void f19 (void);
#pragma omp declare variant (f13) match (device={kind(any,any)})
void f20 (void);
#pragma omp declare variant (f13) match (device={kind(host,nohost)})
void f21 (void);
#pragma omp declare variant (f13) match (device={kind("cpu","gpu","fpga")})
void f22 (void);
#pragma omp declare variant (f13) match (device={kind(any,cpu,nohost)})
void f23 (void);
#pragma omp declare variant (f13) match (device={isa(avx)})
void f24 (void);
#pragma omp declare variant (f13) match (device={isa(sse4,"avx512f",avx512vl,avx512bw)})
void f25 (void);
#pragma omp declare variant (f13) match (device={arch("x86_64")})
void f26 (void);
#pragma omp declare variant (f13) match (device={arch(riscv64)})
void f27 (void);
#pragma omp declare variant (f13) match (device={arch(nvptx)})
void f28 (void);
#pragma omp declare variant (f13) match (device={arch(x86_64),isa("avx512f","avx512vl"),kind(cpu)})
void f29 (void);
#pragma omp declare variant (f13) match (implementation={vendor(amd)})
void f30 (void);
#pragma omp declare variant (f13) match (implementation={vendor(arm)})
void f31 (void);
#pragma omp declare variant (f13) match (implementation={vendor("bsc")})
void f32 (void);
#pragma omp declare variant (f13) match (implementation={vendor(cray)})
void f33 (void);
#pragma omp declare variant (f13) match (implementation={vendor(fujitsu)})
void f34 (void);
#pragma omp declare variant (f13) match (implementation={vendor(gnu)})
void f35 (void);
#pragma omp declare variant (f13) match (implementation={vendor(ibm)})
void f36 (void);
#pragma omp declare variant (f13) match (implementation={vendor("intel")})
void f37 (void);
#pragma omp declare variant (f13) match (implementation={vendor(llvm)})
void f38 (void);
#pragma omp declare variant (f13) match (implementation={vendor(pgi)})
void f39 (void);
#pragma omp declare variant (f13) match (implementation={vendor(ti)})
void f40 (void);
#pragma omp declare variant (f13) match (implementation={vendor(unknown)})
void f41 (void);
#pragma omp declare variant (f13) match (implementation={vendor(gnu,llvm,intel,ibm)})
void f42 (void);
#pragma omp declare variant (f13) match (implementation={extension(my_cute_extension)})	/* { dg-warning "unknown property 'my_cute_extension' of 'extension' selector" } */
void f43 (void);
#pragma omp declare variant (f13) match (implementation={extension(some_other_ext,another_ext)})	/* { dg-warning "unknown property 'some_other_ext' of 'extension' selector" } */
void f44 (void);											/* { dg-warning "unknown property 'another_ext' of 'extension' selector" "" { target *-*-* } .-1 } */
#pragma omp declare variant (f13) match (implementation={unified_shared_memory})
void f45 (void);
#pragma omp declare variant (f13) match (implementation={unified_address})
void f46 (void);
#pragma omp declare variant (f13) match (implementation={dynamic_allocators})
void f47 (void);
#pragma omp declare variant (f13) match (implementation={reverse_offload})
void f48 (void);
#pragma omp declare variant (f13) match (implementation={atomic_default_mem_order(seq_cst)})
void f49 (void);
#pragma omp declare variant (f13) match (implementation={atomic_default_mem_order(relaxed)})
void f50 (void);
#pragma omp declare variant (f13) match (implementation={atomic_default_mem_order(acq_rel)})
void f51 (void);
#pragma omp declare variant (f14) match (implementation={atomic_default_mem_order(acq_rel),vendor(gnu),unified_address,extension(foobar)})	/* { dg-warning "unknown property 'foobar' of 'extension' selector" } */
void f52 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(3):amd)})
void f53 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(4):"arm")})
void f54 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(5):bsc)})
void f55 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(6):cray)})
void f56 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(7):fujitsu)})
void f57 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(8):gnu)})
void f58 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(9):ibm)})
void f59 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(10):intel)})
void f60 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(11):llvm)})
void f61 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(12):pgi)})
void f62 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(13):"ti")})
void f63 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(14):unknown)})
void f64 (void);
#pragma omp declare variant (f13) match (implementation={vendor(score(15):gnu,llvm,intel,ibm)})
void f65 (void);
#pragma omp declare variant (f13) match (implementation={extension(score(16):my_cute_extension)})	/* { dg-warning "unknown property 'my_cute_extension' of 'extension' selector" } */
void f66 (void);
#pragma omp declare variant (f13) match (implementation={extension(score(17):some_other_ext,another_ext)})	/* { dg-warning "unknown property 'some_other_ext' of 'extension' selector" } */
void f67 (void);												/* { dg-warning "unknown property 'another_ext' of 'extension' selector" "" { target *-*-* } .-1 } */
#pragma omp declare variant (f13) match (implementation={atomic_default_mem_order(score(18):seq_cst)})
void f68 (void);
#pragma omp declare variant (f13) match (implementation={atomic_default_mem_order(score(19):relaxed)})
void f69 (void);
#pragma omp declare variant (f13) match (implementation={atomic_default_mem_order(score(20):acq_rel)})
void f70 (void);
#pragma omp declare variant (f13) match (implementation={atomic_default_mem_order(score(21):acq_rel),vendor(score(22):gnu),unified_address,extension(score(22):foobar)})	/* { dg-warning "unknown property 'foobar' of 'extension' selector" } */
void f71 (void);
#pragma omp declare variant (f13) match (user={condition(0)})
void f72 (void);
#pragma omp declare variant (f13) match (user={condition(272-272*1)})
void f73 (void);
#pragma omp declare variant (f13) match (user={condition(score(25):1)})
void f74 (void);
#pragma omp declare variant (f13) match (device={kind(any,"any")})
void f75 (void);
#pragma omp declare variant (f13) match (device={kind("any","any")})
void f76 (void);
#pragma omp declare variant (f13) match (device={kind("any",any)})
void f77 (void);
#pragma omp declare variant (f13) match (implementation={vendor(nvidia)})
void f78 (void);
#pragma omp declare variant (f13) match (user={condition(score(0):0)})
void f79 (void);
