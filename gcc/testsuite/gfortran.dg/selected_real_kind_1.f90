! { dg-do compile }
!
! PR fortran/44347 - arguments of SELECTED_REAL_KIND shall be scalar
! Testcase contributed by Vittorio Zecca <zeccav AT gmail DOT com>
!

  dimension ip(1), ir(1)
  i = selected_real_kind(ip, i)      ! { dg-error "must be a scalar" }
  j = selected_real_kind(i, ir)      ! { dg-error "must be a scalar" }
end
