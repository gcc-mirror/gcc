! { dg-do compile }
! { dg-options "-Ofast" }
  SUBROUTINE foo (a,b,c,d,trigs,inc1,inc2,inc3,inc4,lot,n,la)
    IMPLICIT NONE (type, external)
    INTEGER, PARAMETER ::   wp = 8
    INTEGER, PARAMETER ::  iwp = 4
    INTEGER(iwp) ::  inc1
    INTEGER(iwp) ::  inc2
    INTEGER(iwp) ::  inc3
    INTEGER(iwp) ::  inc4
    INTEGER(iwp) ::  la
    INTEGER(iwp) ::  lot
    INTEGER(iwp) ::  n

    REAL(wp) ::  a(*)
    REAL(wp) ::  b(*)
    REAL(wp) ::  c(*)
    REAL(wp) ::  d(*)
    REAL(wp) ::  trigs(*)

    REAL(wp) ::  c1
    REAL(wp) ::  c2
    REAL(wp) ::  s1
    REAL(wp) ::  s2
    REAL(wp) ::  sin60

    INTEGER(iwp) ::  i
    INTEGER(iwp) ::  ia
    INTEGER(iwp) ::  ib
    INTEGER(iwp) ::  ibase
    INTEGER(iwp) ::  ic
    INTEGER(iwp) ::  iink
    INTEGER(iwp) ::  ijk
    INTEGER(iwp) ::  j
    INTEGER(iwp) ::  ja
    INTEGER(iwp) ::  jb
    INTEGER(iwp) ::  jbase
    INTEGER(iwp) ::  jc
    INTEGER(iwp) ::  jink
    INTEGER(iwp) ::  jump
    INTEGER(iwp) ::  k
    INTEGER(iwp) ::  kb
    INTEGER(iwp) ::  kc
    INTEGER(iwp) ::  kstop
    INTEGER(iwp) ::  l
    INTEGER(iwp) ::  m

    sin60=0.866025403784437_wp

    ia = 1
    ib = ia + (2*m-la)*inc1
    ic = ib
    ja = 1
    jb = ja + jink
    jc = jb + jink

    DO k = la, kstop, la
       kb = k + k
       kc = kb + kb
       c1 = trigs(kb+1)
       s1 = trigs(kb+2)
       c2 = trigs(kc+1)
       s2 = trigs(kc+2)
       ibase = 0
       DO l = 1, la
          i = ibase
          j = jbase
          DO ijk = 1, lot
             c(ja+j) = a(ia+i) + (a(ib+i)+a(ic+i))
             d(ja+j) = b(ia+i) + (b(ib+i)-b(ic+i))
             c(jb+j) = c1*((a(ia+i)-0.5_wp*(a(ib+i)+a(ic+i)))-(sin60*(b(ib+i)+ &
                  &            b(ic+i))))                                      &
                  &    - s1*((b(ia+i)-0.5_wp*(b(ib+i)-b(ic+i)))+(sin60*(a(ib+i)- &
                  &            a(ic+i))))
             d(jb+j) = s1*((a(ia+i)-0.5_wp*(a(ib+i)+a(ic+i)))-(sin60*(b(ib+i)+ &
                  &            b(ic+i))))                                      &
                  &    + c1*((b(ia+i)-0.5_wp*(b(ib+i)-b(ic+i)))+(sin60*(a(ib+i)- &
                  &            a(ic+i))))
             c(jc+j) = c2*((a(ia+i)-0.5_wp*(a(ib+i)+a(ic+i)))+(sin60*(b(ib+i)+ &
                  &            b(ic+i))))                                      &
                  &    - s2*((b(ia+i)-0.5_wp*(b(ib+i)-b(ic+i)))-(sin60*(a(ib+i)- &
                  &            a(ic+i))))
             i = i + inc3
             j = j + inc4
          END DO
          ibase = ibase + inc1
          jbase = jbase + inc2
       END DO
       ia = ia + iink
       ib = ib + iink
       ic = ic - iink
       jbase = jbase + jump
    END DO
  END
