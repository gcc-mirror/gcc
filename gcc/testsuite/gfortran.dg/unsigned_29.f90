! { dg-do run }
! { dg-options "-funsigned" }
program memain
  implicit none
  call test1
  call test2
contains
  subroutine test1
    unsigned, dimension(2,2) :: v
    integer(8), dimension(2,2) :: i
    v = reshape([4278255360u, 4042322160u, 3435973836u, 2863311530u],[2,2])
    i = int(v,8)
    if (iall(v) /= 2147516416u) error stop 1
    if (iany(v) /= 4294901758u) error stop 2
    if (iparity(v) /= 1771465110u) error stop 3
    if (any(iall(v,dim=1) /= [4026593280u, 2290649224u])) error stop 4
    if (any(iall(v,dim=2) /= [3422604288u, 2694881440u])) error stop 5
    if (any(iany(v,dim=1) /= [4293984240u, 4008636142u])) error stop 6
    if (any(iany(v,dim=2) /= [4291624908u, 4210752250u])) error stop 7
    if (any(iparity(v,dim=1) /= [267390960u, 1717986918u])) error stop 8
    if (any(iparity(v,dim=2) /= [869020620u, 1515870810u])) error stop 9
  end subroutine test1
  subroutine test2
    unsigned, dimension(2,2), parameter :: v &
         = reshape([4278255360u, 4042322160u, 3435973836u, 2863311530u],[2,2])
    unsigned, parameter :: v_all = iall(v), v_any = iany(v), v_parity = iparity(v)
    unsigned, parameter, dimension(2) :: v_all_1 = iall(v,dim=1), v_all_2 = iall(v,dim=2)
    unsigned, parameter, dimension(2) :: v_any_1 = iany(v,dim=1), v_any_2 = iany(v,dim=2)
    unsigned, parameter, dimension(2) :: v_parity_1 = iparity(v,dim=1), v_parity_2 = iparity(v,dim=2)
    if (v_all /= 2147516416u) error stop 10
    if (v_any /= 4294901758u) error stop 11
    if (v_parity /= 1771465110u) error stop 12
    if (any(v_all_1 /= [4026593280u, 2290649224u])) error stop 13
    if (any(v_all_2 /= [3422604288u, 2694881440u])) error stop 14
    if (any(v_any_1 /= [4293984240u, 4008636142u])) error stop 15
    if (any(v_any_2 /= [4291624908u, 4210752250u])) error stop 16
    if (any(v_parity_1 /= [267390960u, 1717986918u])) error stop 17
    if (any(v_parity_2 /= [869020620u, 1515870810u])) error stop 18
  end subroutine test2
end program memain
