! Ensure that loops not affiliated with acc compute regions cause an error.

subroutine test1
    !$acc loop gang ! { dg-error "loop directive must be associated with an OpenACC compute region" }
  DO i = 1,10
  ENDDO
end subroutine test1
