!pr 12839- F2003 formatting of Inf /Nan 
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


! check a field width < 3
       fmt = '(F2.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'**') call abort
       write(l,fmt=fmt)neg_inf
       if (l.ne.'**') call abort
       write(l,fmt=fmt)nan
       if (l.ne.'**') call abort

! check a field width = 3
       fmt = '(F3.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'Inf') call abort
       write(l,fmt=fmt)neg_inf
       if (l.ne.'Inf') call abort
       write(l,fmt=fmt)nan
       if (l.ne.'NaN') call abort

! check a field width > 3
       fmt = '(F4.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'+Inf') call abort
       write(l,fmt=fmt)neg_inf
       if (l.ne.'-Inf') call abort
       write(l,fmt=fmt)nan
       if (l.ne.' NaN') call abort

! check a field width = 7
       fmt = '(F7.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'   +Inf') call abort
       write(l,fmt=fmt)neg_inf
       if (l.ne.'   -Inf') call abort
       write(l,fmt=fmt)nan
       if (l.ne.'    NaN') call abort

! check a field width = 8
       fmt = '(F8.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'Infinity') call abort
       write(l,fmt=fmt)neg_inf
       if (l.ne.'Infinity') call abort
       write(l,fmt=fmt)nan
       if (l.ne.'     NaN') call abort

! check a field width = 9
       fmt = '(F9.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'+Infinity') call abort
       write(l,fmt=fmt)neg_inf
       if (l.ne.'-Infinity') call abort
       write(l,fmt=fmt)nan
       if (l.ne.'      NaN') call abort

! check a field width = 14
       fmt = '(F14.0)'
       write(l,fmt=fmt)pos_inf
       if (l.ne.'     +Infinity') call abort
       write(l,fmt=fmt)neg_inf
       if (l.ne.'     -Infinity') call abort
       write(l,fmt=fmt)nan
       if (l.ne.'           NaN') call abort
       end

