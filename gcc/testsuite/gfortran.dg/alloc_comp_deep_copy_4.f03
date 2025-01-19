!{ dg-do run }
!
! Contributed Vladimir Terzi  <vterzi1996@gmail.com>
! Check that deep-copy for b=a works.

program pr114672
    type node
        integer::val
        type(node),allocatable::next
    end type

    type(node)::a,b

    allocate(a%next)
    a%val=1
    a%next%val=2
!    print*,a%val,a%next%val
    b=a
    b%val=3
    b%next%val=4
    if (loc(b) == loc(a)) stop 1
    if (loc(b%next) == loc(a%next)) stop 2
!    print*,a%val,a%next%val
    deallocate(b%next)
    if (.NOT. allocated(a%next)) stop 3
!    print*,a%val,a%next%val
    deallocate(a%next)
end

