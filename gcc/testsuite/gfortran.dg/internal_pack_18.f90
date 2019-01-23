! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! PR 57992 - this was packed/unpacked unnecessarily.
! Original case by Tobias Burnus.
subroutine test
  interface
    function f2()
      integer, pointer, contiguous :: f2(:)
    end function f2
  end interface

 call bar(f2())
end subroutine test
! { dg-final { scan-tree-dump-not "_gfortran_internal_pack" "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_internal_unpack" "original" } }
