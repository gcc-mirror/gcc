! Verify the accuracy of the line number associated with combined constructs.
! See "../../c-c++-common/goacc/combined-directives-3.c".

subroutine test
  implicit none
  integer x, y, z

  !$acc parallel loop seq auto ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
  do x = 0, 10
     !$acc loop
     do y = 0, 10
     end do
  end do
  !$acc end parallel loop

  !$acc parallel loop gang auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
  do x = 0, 10
     !$acc loop worker auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
     do y = 0, 10
        !$acc loop vector
        do z = 0, 10
        end do
     end do
  end do
  !$acc end parallel loop
end subroutine test
