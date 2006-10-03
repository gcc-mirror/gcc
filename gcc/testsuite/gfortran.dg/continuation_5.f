! { dg-do compile }
! { dg-options -pedantic }
! PR 19262  Test limit on line continuations. Test case derived form case in PR
! by Steve Kargl.  Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
       print *,
     c "1" // !  1
     c "2" // !  2
     c "3" // !  3
     c "4" // !  4
     c "5" // !  5
     c "6" // !  6
     c "7" // !  7
     c "8" // !  8
     c "9" // !  9
     c "0" // ! 10
     c "1" // ! 11
     c "2" // ! 12
     c "3" // ! 13
     c "4" // ! 14
     c "5" // ! 15
     c "6" // ! 16
     c "7" // ! 17
     c "8" // ! 18
     c "9"    ! 19
       print *,
     c "1" // !  1
     c "2" // !  2
     c "3" // !  3
     c "4" // !  4
     c "5" // !  5
     c "6" // !  6
     c "7" // !  7
     c "8" // !  8
     c "9" // !  9
     c "0" // ! 10
     c "1" // ! 11
     c "2" // ! 12
     c "3" // ! 13
     c "4" // ! 14
     c "5" // ! 15
     c "6" // ! 16
     c "7" // ! 17
     c "8" // ! 18
     c "9" // ! 19
     c "0"    ! { dg-warning "Limit of 19 continuations exceeded" }
       end