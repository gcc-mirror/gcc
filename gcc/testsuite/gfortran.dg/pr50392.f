! { dg-do compile }
! 
      function kf()
      integer kf
      assign 1 to kf ! { dg-warning "Deleted feature: ASSIGN statement at" }
      kf = 2
      goto kf ! { dg-warning "Deleted feature: Assigned GOTO statement at" }
      kf = 1
 1    continue
      kf = 0
      end

