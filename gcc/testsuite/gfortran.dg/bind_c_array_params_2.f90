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

! { dg-final { scan-assembler-times "myBindC" 1 { target { ! { hppa*-*-hpux* } } } } }
! { dg-final { scan-assembler-times "myBindC,%r2" 1 { target { hppa*-*-hpux* } } } }
! { dg-final { scan-tree-dump-times "test \\\(&parm\\." 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
