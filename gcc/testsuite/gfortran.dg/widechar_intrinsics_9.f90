! { dg-do run }
! { dg-options "-fbackslash" }

  implicit none
  character(kind=1,len=3) :: s1, t1
  character(kind=4,len=3) :: s4, t4

  s1 = "foo" ; t1 = "bar"
  call check_minmax_1 ("foo", "bar", min("foo","bar"), max("foo","bar"))
  call check_minmax_1 ("bar", "foo", min("foo","bar"), max("foo","bar"))
  call check_minmax_1 (s1, t1, min(s1,t1), max(s1,t1))
  call check_minmax_1 (t1, s1, min(s1,t1), max(s1,t1))

  s1 = "   " ; t1 = "bar"
  call check_minmax_1 ("   ", "bar", min("   ","bar"), max("   ","bar"))
  call check_minmax_1 ("bar", "   ", min("   ","bar"), max("   ","bar"))
  call check_minmax_1 (s1, t1, min(s1,t1), max(s1,t1))
  call check_minmax_1 (t1, s1, min(s1,t1), max(s1,t1))

  s1 = "   " ; t1 = "   "
  call check_minmax_1 ("   ", "   ", min("   ","   "), max("   ","   "))
  call check_minmax_1 ("   ", "   ", min("   ","   "), max("   ","   "))
  call check_minmax_1 (s1, t1, min(s1,t1), max(s1,t1))
  call check_minmax_1 (t1, s1, min(s1,t1), max(s1,t1))

  s1 = "d\xFF " ; t1 = "d  "
  call check_minmax_1 ("d\xFF ", "d  ", min("d\xFF ","d  "), max("d\xFF ","d  "))
  call check_minmax_1 ("d  ", "d\xFF ", min("d\xFF ","d  "), max("d\xFF ","d  "))
  call check_minmax_1 (s1, t1, min(s1,t1), max(s1,t1))
  call check_minmax_1 (t1, s1, min(s1,t1), max(s1,t1))

  s4 = 4_"   " ; t4 = 4_"xxx"
  call check_minmax_2 (4_"   ", 4_"xxx", min(4_"   ", 4_"xxx"), &
                       max(4_"   ", 4_"xxx"))
  call check_minmax_2 (4_"xxx", 4_"   ", min(4_"   ", 4_"xxx"), &
                       max(4_"   ", 4_"xxx"))
  call check_minmax_2 (s4, t4, min(s4,t4), max(s4,t4))
  call check_minmax_2 (t4, s4, min(s4,t4), max(s4,t4))

  s4 = 4_" \u1be3m" ; t4 = 4_"xxx"
  call check_minmax_2 (4_" \u1be3m", 4_"xxx", min(4_" \u1be3m", 4_"xxx"), &
                       max(4_" \u1be3m", 4_"xxx"))
  call check_minmax_2 (4_"xxx", 4_" \u1be3m", min(4_" \u1be3m", 4_"xxx"), &
                       max(4_" \u1be3m", 4_"xxx"))
  call check_minmax_2 (s4, t4, min(s4,t4), max(s4,t4))
  call check_minmax_2 (t4, s4, min(s4,t4), max(s4,t4))

contains

  subroutine check_minmax_1 (s1, s2, smin, smax)
    implicit none
    character(kind=1,len=*), intent(in) :: s1, s2, smin, smax
    character(kind=4,len=len(s1)) :: w1, w2, wmin, wmax

    w1 = s1 ; w2 = s2 ; wmin = smin ; wmax = smax
    if (min (w1, w2) /= wmin) call abort
    if (max (w1, w2) /= wmax) call abort
    if (min (s1, s2) /= smin) call abort
    if (max (s1, s2) /= smax) call abort
  end subroutine check_minmax_1

  subroutine check_minmax_2 (s1, s2, smin, smax)
    implicit none
    character(kind=4,len=*), intent(in) :: s1, s2, smin, smax

    if (min (s1, s2) /= smin) call abort
    if (max (s1, s2) /= smax) call abort
  end subroutine check_minmax_2

end
