! This test case ensures that the -MT flag is correctly replacing the object name in the dependency file.
! See PR 47485
!
! Contributed by Vincent Vanlaer <vincenttc@volkihar.be>
!
! { dg-do preprocess }
! { dg-additional-options "-cpp" }
! { dg-additional-options "-M" }
! { dg-additional-options "-MF deps" }
! { dg-additional-options "-MT obj.o" }

module test
end module

! { dg-final { scan-file "deps" "obj.o:.*" } }
