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

! { dg-final { scan-assembler-times "\[ \t\]\[$,_0-9\]*myBindC" 1 { target { ! { hppa*-*-* s390*-*-* *-*-cygwin* amdgcn*-*-* powerpc-ibm-aix*} } } } }
! { dg-final { scan-assembler-times "myBindC,%r2" 1 { target { hppa*-*-* } } } }
! { dg-final { scan-assembler-times "call\tmyBindC" 1 { target { *-*-cygwin* } } } }
! { dg-final { scan-assembler-times "brasl\t%r\[0-9\]*,myBindC" 1 { target { s390*-*-* } } } }
! { dg-final { scan-assembler-times "bl \.myBindC" 1 { target { powerpc-ibm-aix* } } } }
! { dg-final { scan-assembler-times "add_u32\t\[sv\]\[0-9\]*, \[sv\]\[0-9\]*, myBindC@rel32@lo" 1 { target { amdgcn*-*-* } } } }


! { dg-final { scan-tree-dump "parm...span = 4;" "original" } }
! { dg-final { scan-tree-dump "parm...dtype = {.elem_len=4, .rank=2, .type=1};" "original" } }
! { dg-final { scan-tree-dump "parm...dim\\\[0\\\].lbound = 1;" "original" } }
! { dg-final { scan-tree-dump "parm...dim\\\[0\\\].ubound = 4;" "original" } }
! { dg-final { scan-tree-dump "parm...dim\\\[0\\\].stride = 1;" "original" } }
! { dg-final { scan-tree-dump "parm...dim\\\[1\\\].lbound = 1;" "original" } }
! { dg-final { scan-tree-dump "parm...dim\\\[1\\\].ubound = 4;" "original" } }
! { dg-final { scan-tree-dump "parm...dim\\\[1\\\].stride = 4;" "original" } }
! { dg-final { scan-tree-dump "parm...data = \\(void \\*\\) &aa\\\[0\\\];" "original" } }
! { dg-final { scan-tree-dump "parm...offset = -5;" "original" } }
! { dg-final { scan-tree-dump "cfi...version = 1;" "original" } }
! { dg-final { scan-tree-dump "cfi...rank = 2;" "original" } }
! { dg-final { scan-tree-dump "cfi...type = 1025;" "original" } }
! { dg-final { scan-tree-dump "cfi...attribute = 2;" "original" } }
! { dg-final { scan-tree-dump "cfi...base_addr = parm.0.data;" "original" } }
! { dg-final { scan-tree-dump "cfi...elem_len = 4;" "original" } }
! { dg-final { scan-tree-dump "idx.2 = 0;" "original" } }

! { dg-final { scan-tree-dump "if \\(idx.. <= 1\\) goto L..;" "original" } }
! { dg-final { scan-tree-dump "cfi...dim\\\[idx..\\\].lower_bound = 0;" "original" } }
! { dg-final { scan-tree-dump "cfi...dim\\\[idx..\\\].extent = \\(parm...dim\\\[idx..\\\].ubound - parm...dim\\\[idx..\\\].lbound\\) \\+ 1;" "original" } }
! { dg-final { scan-tree-dump "cfi...dim\\\[idx..\\\].sm = parm...dim\\\[idx..\\\].stride \\* parm...span;" "original" } }
! { dg-final { scan-tree-dump "idx.. = idx.. \\+ 1;" "original" } }

! { dg-final { scan-tree-dump "test \\(&cfi..\\);" "original" } }


