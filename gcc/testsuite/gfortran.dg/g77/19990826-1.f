c { dg-do compile }
* Date: Tue, 24 Aug 1999 12:25:41 +1200 (NZST)
* From: Jonathan Ravens <ravens@whio.gns.cri.nz>
* To: gcc-bugs@gcc.gnu.org
* Subject: g77 bug report
* X-UIDL: a0bf5ecc21487cde48d9104983ab04d6

! This fortran source will not compile - if the penultimate elseif block is 0
! included then the message appears :
!
!   /usr/src/egcs//gcc-2.95.1/gcc/f/stw.c:308: failed assertion `b->uses_ > 0'
!   g77: Internal compiler error: program f771 got fatal signal 6
!
! The command was : g77 -c <prog.f>
!
! The OS is Red Hat 6, and the output from uname -a is 
!   Linux grfw1452.gns.cri.nz 2.2.5-15 #1 Mon Apr 19 23:00:46 EDT 1999 i686 unknown
!
! The configure script I used was 
!   /usr/src/egcs/gcc/gcc-2.95.1/configure --enable-languages=f77 i585-unknown-linux
!
! I was installing 2.95 because under EGCS 2.1.1 none of my code was working
! with optimisation turned on, and there were still bugs with no optimisation
! (all of which code works fine under g77 0.5.21 and Sun/IBM/Dec/HP fortrans).
!
! The version of g77 is :
!
!g77 version 2.95.1 19990816 (release) (from FSF-g77 version 0.5.25 19990816 (release))

        program main
        if (i.eq.1) then
            call abc(1)
        else if (i.eq. 1) then
            call abc( 1)
        else if (i.eq. 2) then
            call abc( 2)
        else if (i.eq. 3) then
            call abc( 3)
        else if (i.eq. 4) then
            call abc( 4)
        else if (i.eq. 5) then
            call abc( 5)
        else if (i.eq. 6) then
            call abc( 6)
        else if (i.eq. 7) then
            call abc( 7)
        else if (i.eq. 8) then
            call abc( 8)
        else if (i.eq. 9) then
            call abc( 9)
        else if (i.eq. 10) then
            call abc( 10)
        else if (i.eq. 11) then
            call abc( 11)
        else if (i.eq. 12) then
            call abc( 12)
        else if (i.eq. 13) then
            call abc( 13)
        else if (i.eq. 14) then
            call abc( 14)
        else if (i.eq. 15) then
            call abc( 15)
        else if (i.eq. 16) then
            call abc( 16)
        else if (i.eq. 17) then
            call abc( 17)
        else if (i.eq. 18) then
            call abc( 18)
        else if (i.eq. 19) then
            call abc( 19)
        else if (i.eq. 20) then
            call abc( 20)
        else if (i.eq. 21) then
            call abc( 21)
        else if (i.eq. 22) then
            call abc( 22)
        else if (i.eq. 23) then
            call abc( 23)
        else if (i.eq. 24) then
            call abc( 24)
        else if (i.eq. 25) then
            call abc( 25)
        else if (i.eq. 26) then
            call abc( 26)
        else if (i.eq. 27) then
            call abc( 27)
        else if (i.eq. 28) then
            call abc( 28)
        else if (i.eq. 29) then
            call abc( 29)
        else if (i.eq. 30) then
            call abc( 30)
        else if (i.eq. 31) then
            call abc( 31)
        else if (i.eq. 32) then
            call abc( 32)
        else if (i.eq. 33) then
            call abc( 33)
        else if (i.eq. 34) then
            call abc( 34)
        else if (i.eq. 35) then
            call abc( 35)
        else if (i.eq. 36) then
            call abc( 36)
        else if (i.eq. 37) then
            call abc( 37)
        else if (i.eq. 38) then
            call abc( 38)
        else if (i.eq. 39) then
            call abc( 39)
        else if (i.eq. 40) then
            call abc( 40)
        else if (i.eq. 41) then
            call abc( 41)
        else if (i.eq. 42) then
            call abc( 42)
        else if (i.eq. 43) then
            call abc( 43)
        else if (i.eq. 44) then
            call abc( 44)
        else if (i.eq. 45) then
            call abc( 45)
        else if (i.eq. 46) then
            call abc( 46)
        else if (i.eq. 47) then
            call abc( 47)
        else if (i.eq. 48) then
            call abc( 48)
        else if (i.eq. 49) then
            call abc( 49)
        else if (i.eq. 50) then
            call abc( 50)
        else if (i.eq. 51) then
            call abc( 51)
        else if (i.eq. 52) then
            call abc( 52)
        else if (i.eq. 53) then
            call abc( 53)
        else if (i.eq. 54) then
            call abc( 54)
        else if (i.eq. 55) then
            call abc( 55)
        else if (i.eq. 56) then
            call abc( 56)
        else if (i.eq. 57) then
            call abc( 57)
        else if (i.eq. 58) then
            call abc( 58)
        else if (i.eq. 59) then
            call abc( 59)
        else if (i.eq. 60) then
            call abc( 60)
        else if (i.eq. 61) then
            call abc( 61)
        else if (i.eq. 62) then
            call abc( 62)
        else if (i.eq. 63) then
            call abc( 63)
        else if (i.eq. 64) then
            call abc( 64)
        else if (i.eq. 65) then
            call abc( 65)
        else if (i.eq. 66) then
            call abc( 66)
        else if (i.eq. 67) then
            call abc( 67)
        else if (i.eq. 68) then
            call abc( 68)
        else if (i.eq. 69) then
            call abc( 69)
        else if (i.eq. 70) then
            call abc( 70)
        else if (i.eq. 71) then
            call abc( 71)
        else if (i.eq. 72) then
            call abc( 72)
        else if (i.eq. 73) then
            call abc( 73)
        else if (i.eq. 74) then
            call abc( 74)
        else if (i.eq. 75) then
            call abc( 75)
        else if (i.eq. 76) then
            call abc( 76)
        else if (i.eq. 77) then
            call abc( 77)
        else if (i.eq. 78) then
            call abc( 78)
        else if (i.eq. 79) then
            call abc( 79)
        else if (i.eq. 80) then
            call abc( 80)
        else if (i.eq. 81) then
            call abc( 81)
        else if (i.eq. 82) then
            call abc( 82)
        else if (i.eq. 83) then
            call abc( 83)
        else if (i.eq. 84) then
            call abc( 84)
        else if (i.eq. 85) then
            call abc( 85)
        else if (i.eq. 86) then
            call abc( 86)
        else if (i.eq. 87) then
            call abc( 87)
        else if (i.eq. 88) then
            call abc( 88)
        else if (i.eq. 89) then
            call abc( 89)
        else if (i.eq. 90) then
            call abc( 90)
        else if (i.eq. 91) then
            call abc( 91)
        else if (i.eq. 92) then
            call abc( 92)
        else if (i.eq. 93) then
            call abc( 93)
        else if (i.eq. 94) then
            call abc( 94)
        else if (i.eq. 95) then
            call abc( 95)
        else if (i.eq. 96) then
            call abc( 96)
        else if (i.eq. 97) then
            call abc( 97)
        else if (i.eq. 98) then
            call abc( 98)
        else if (i.eq. 99) then
            call abc( 99)
        else if (i.eq. 100) then
            call abc( 100)
        else if (i.eq. 101) then
            call abc( 101)
        else if (i.eq. 102) then
            call abc( 102)
        else if (i.eq. 103) then
            call abc( 103)
        else if (i.eq. 104) then
            call abc( 104)
        else if (i.eq. 105) then
            call abc( 105)
        else if (i.eq. 106) then
            call abc( 106)
        else if (i.eq. 107) then
            call abc( 107)
        else if (i.eq. 108) then
            call abc( 108)
        else if (i.eq. 109) then
            call abc( 109)
        else if (i.eq. 110) then
            call abc( 110)
        else if (i.eq. 111) then
            call abc( 111)
        else if (i.eq. 112) then
            call abc( 112)
        else if (i.eq. 113) then
            call abc( 113)
        else if (i.eq. 114) then
            call abc( 114)
        else if (i.eq. 115) then
            call abc( 115)
        else if (i.eq. 116) then
            call abc( 116)
        else if (i.eq. 117) then
            call abc( 117)
        else if (i.eq. 118) then
            call abc( 118)
        else if (i.eq. 119) then
            call abc( 119)
        else if (i.eq. 120) then
            call abc( 120)
        else if (i.eq. 121) then
            call abc( 121)
        else if (i.eq. 122) then
            call abc( 122)
        else if (i.eq. 123) then
            call abc( 123)
        else if (i.eq. 124) then
            call abc( 124)
        else if (i.eq. 125) then           !< Miscompiles if present
            call abc( 125)                 !<

c        else if (i.eq. 126) then
c            call abc( 126)
        endif
        end
