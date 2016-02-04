! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }
!
! Run-time test for memory consistency
!
! Contributed by Deepak Eachempati

program cp_bug
    implicit none
    integer :: v1, v2, u[*]
    integer :: me

    me = this_image()

    u = 0
    v1 = 10

    v1 = u[me]

    ! v2 should get value in u (0)
    v2 = v1

    if(v2 /= u) call abort()

end program
