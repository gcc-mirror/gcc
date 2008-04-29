   subroutine foo(func,p,eval)
      real(kind=kind(1.0d0)), dimension(3,0:4,0:4,0:4) :: p
      logical(kind=kind(.true.)), dimension(5,5,5) :: eval
      interface
         subroutine func(values,pt)
            real(kind=kind(1.0d0)), dimension(:), intent(out) :: values
            real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: pt
         end subroutine
      end interface
      real(kind=kind(1.0d0)), dimension(125,3) :: pt
      integer(kind=kind(1)) :: n_pt

      n_pt = 1
      pt(1:n_pt,:) = &
         reshape( &
            pack( &
               transpose(reshape(p,(/3,125/))), &
               spread(reshape(eval,(/125/)),dim=2,ncopies=3)), &
            (/n_pt,3/))

   end subroutine
   end 
