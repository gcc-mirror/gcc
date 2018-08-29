!pr 12839- F2003 formatting of Inf /Nan 
! Modified for PR47434
       implicit none
       character*40 l
       character*12 fmt
       real zero, pos_inf, neg_inf, nan
       zero = 0.0

! need a better way of generating these floating point
! exceptional constants.

       pos_inf =  1.0/zero
       neg_inf = -1.0/zero
       nan = zero/zero

! check a field width = 0
       fmt = '(F0.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'Inf') STOP 1
       write(l,fmt=fmt)neg_inf
       if (l.ne.'-Inf') STOP 2
       write(l,fmt=fmt)nan
       if (l.ne.'NaN') STOP 3

! check a field width < 3
       fmt = '(F2.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'**') STOP 4
       write(l,fmt=fmt)neg_inf
       if (l.ne.'**') STOP 5
       write(l,fmt=fmt)nan
       if (l.ne.'**') STOP 6

! check a field width = 3
       fmt = '(F3.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'Inf') STOP 7
       write(l,fmt=fmt)neg_inf
       if (l.ne.'***') STOP 8
       write(l,fmt=fmt)nan
       if (l.ne.'NaN') STOP 9

! check a field width > 3
       fmt = '(F4.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.' Inf') STOP 10
       write(l,fmt=fmt)neg_inf
       if (l.ne.'-Inf') STOP 11
       write(l,fmt=fmt)nan
       if (l.ne.' NaN') STOP 12

! check a field width = 7
       fmt = '(F7.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'    Inf') STOP 13
       write(l,fmt=fmt)neg_inf
       if (l.ne.'   -Inf') STOP 14
       write(l,fmt=fmt)nan
       if (l.ne.'    NaN') STOP 15

! check a field width = 8
       fmt = '(F8.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'Infinity') STOP 16
       write(l,fmt=fmt)neg_inf
       if (l.ne.'    -Inf') STOP 17
       write(l,fmt=fmt)nan
       if (l.ne.'     NaN') STOP 18

! check a field width = 9
       fmt = '(F9.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.' Infinity') STOP 19
       write(l,fmt=fmt)neg_inf
       if (l.ne.'-Infinity') STOP 20
       write(l,fmt=fmt)nan
       if (l.ne.'      NaN') STOP 21

! check a field width = 14
       fmt = '(F14.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'      Infinity') STOP 22
       write(l,fmt=fmt)neg_inf
       if (l.ne.'     -Infinity') STOP 23
       write(l,fmt=fmt)nan
       if (l.ne.'           NaN') STOP 24
       end

