! { dg-do compile }
! Check the fix for PR20839, which concerned non-compliance with one of the
! constraints for block-do-constructs (8.1.4.1.1):
! Constraint: If the do-stmt of a block-do-construct is identified by a 
! do-construct-name, the corresponding end-do shall be an end-do-stmt
! specifying the same do-construct-name. (Tests a & b)
! If the do-stmt of a block-do-construct is not identified by a
! do-construct-name, the corresponding end-do shall not specify a
! do-construct-name. (Tests c & d)
! Constraint: If the do-stmt is a nonlabel-do-stmt, the corresponding end-do
! shall be an end-do-stmt.
! Constraint: If the do-stmt is a label-do-stmt, the corresponding end-do shall
! be identified with the same label.
!
! Test a - this was the PR
  doi: DO 111 i=1,3 ! { dg-error "requires matching ENDDO name" }
111 continue 
! Test b
  doii: DO 112 ij=1,3
112 enddo doij      ! { dg-error "Expected label" }
! Test c
  DO 113 ik=1,3
113 enddo doik      ! { dg-error "Syntax error" }
! Test d
  DO il=1,3
  enddo doil        ! { dg-error "Syntax error" }
! Test e
  doj: DO 114 j=1,3
  enddo doj         ! { dg-error "doesn't match DO label" }

! Correct block do constructs
dok: DO 115 k=1,3
    dokk: do kk=1,3
        dokkk: DO
                   do kkkk=1,3
                       do
                       enddo
                   enddo
               enddo dokkk
          enddo dokk
115  enddo dok 
! Correct non-block do constructs
  do 117 l=1,3
      do ll=1,3
          do 116 lll=1,3
116       continue
      enddo
117 enddo
! These prevent an EOF error, arising from the previous errors.
end do
113 end do
112 end do doii
END

