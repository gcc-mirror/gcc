! { dg-do compile }
! PR 34305 - make sure there's an error message for specifying a
      program test
      parameter (datasize = 1000) 
      dimension idata (datasize)  ! { dg-error "must be of INTEGER type|must have constant shape" }
      idata (1) = -1
      end
