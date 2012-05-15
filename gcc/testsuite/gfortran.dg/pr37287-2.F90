! PR debug/37287
! { dg-do compile }
! { dg-options "-g" }
module pr37287_2
#ifdef PR37287_1
  use pr37287_1
#endif
  implicit none
end module pr37287_2
