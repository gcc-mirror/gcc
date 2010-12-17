subroutine pr40421 (j, q, r)
  double precision :: q(1,1), r(1,1,3)
  save
  integer :: i, j, m, n
  double precision :: s, t, u
  do i=1,2
    do m=1,j
      do n=1,1
        s=q(n,m)*r(n,m,1)
        t=q(n,m)*r(n,m,2)
        u=q(n,m)*r(n,m,3)
      end do
    end do
  end do
end
