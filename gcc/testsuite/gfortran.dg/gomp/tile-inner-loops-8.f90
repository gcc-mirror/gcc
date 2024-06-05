subroutine test3a
  !$omp parallel do
  !$omp tile sizes(3,3,3,3)
  do i=0,100
    do j=-300,100
      !$omp tile sizes(5) ! { dg-error "TILE construct at \\\(1\\\) generates 1 loops with canonical form but 2 loops are needed" }
      do k=-300,100
        do l=-300,100
          call dummy (l)
        end do
      end do
    end do
  end do
end subroutine test3a

subroutine test4a
  !$omp parallel do
  !$omp tile sizes(3,3,3,3,3)
  do i=0,100
     do j=-300,100
        !$omp tile sizes(5,5) ! { dg-error "TILE construct at \\\(1\\\) generates 2 loops with canonical form but 3 loops are needed" }
        do k=-300,100
           do l=-300,100
           do m=-300,100
              call dummy (m)
           end do
           end do
        end do
     end do
  end do
end subroutine test4a

subroutine test3b
  !$omp parallel do
  !$omp tile sizes(3,3,3,3)
  do i=0,100
    do j=-300,100
      !$omp tile sizes(5) ! { dg-error "TILE construct at \\\(1\\\) generates 1 loops with canonical form but 2 loops are needed" }
      do k=-300,100
        do l=-300,100
          call dummy (l)
        end do
      end do
    end do
  end do
end subroutine test3b

subroutine test4b
  !$omp parallel do
  !$omp tile sizes(3,3,3,3,3)
  do i=0,100
    do j=-300,100
      !$omp tile sizes(5,5) ! { dg-error "TILE construct at \\\(1\\\) generates 2 loops with canonical form but 3 loops are needed" }
      do k=-300,100
        do l=-300,100
          do m=-300,100
            call dummy (m)
          end do
        end do
      end do
    end do
  end do
end subroutine test4b
