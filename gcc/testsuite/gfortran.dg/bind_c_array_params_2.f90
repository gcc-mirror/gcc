! { dg-do compile }
! { dg-options "-std=f2008ts -fdump-tree-original" }
! { dg-additional-options "-mno-explicit-relocs" { target alpha*-*-* } }
! { dg-additional-options "-mno-relax-pic-calls" { target mips*-*-* } }
!
! Check that assumed-shape variables are correctly passed to BIND(C)
! as defined in TS 29913
!
interface
  subroutine test (xx) bind(C, name="myBindC")
    type(*), dimension(:,:) :: xx
  end subroutine test
end interface

integer :: aa(4,4)
call test(aa)
end

! { dg-final { scan-assembler-times "\[ \t\]\[$,_0-9\]*myBindC" 1 { target { ! { hppa*-*-* s390*-*-* *-*-cygwin* amdgcn*-*-* } } } } }
! { dg-final { scan-assembler-times "myBindC,%r2" 1 { target { hppa*-*-* } } } }
! { dg-final { scan-assembler-times "call\tmyBindC" 1 { target { *-*-cygwin* } } } }
! { dg-final { scan-assembler-times "brasl\t%r\[0-9\]*,myBindC" 1 { target { s390*-*-* } } } }
! { dg-final { scan-assembler-times "add_u32\t\[sv\]\[0-9\]*, \[sv\]\[0-9\]*, myBindC@rel32@lo" 1 { target { amdgcn*-*-* } } } }
! { dg-final { scan-tree-dump-times "cfi_desc_to_gfc_desc \\\(&parm\\." 1 "original" } }
