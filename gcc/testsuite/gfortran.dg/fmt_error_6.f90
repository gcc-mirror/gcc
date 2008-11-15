! { dg-do compile }
! { dg-options }
! PR37988 Edit descriptor checking (compile time) for "<Holerith>T)"
! Test case derived from the reporter.
 8001 FORMAT(//,'     SIGNIFICANCE LEVEL =',F7.4, 21H ONE-SIDED AT THE LEFT) ! { dg-error "required with T descriptor" }
      end
