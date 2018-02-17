! Program to test the ASSOCIATED intrinsic with cross-kinds
program intrinsic_associated_2
   logical*4 :: t4, L44, L48
   logical*8 :: t8, L84, L88
   real*4, pointer :: a4p(:, :)
   real*8, pointer :: a8p(:, :)
   real*4, target  :: a4(10, 10)
   real*8, target  :: a8(10, 10)
                                                                                
   t4 = .true.
   t8 = .true.
   t8 = t4
   a4p => a4
   a8p => a8
   L44 = t4 .and. associated (a4p, a4)
   L84 = t8 .and. associated (a4p, a4)
   L48 = t4 .and. associated (a8p, a8)
   L88 = t8 .and. associated (a8p, a8)
   if (.not. (L44 .and. L84 .and. L48 .and. L88)) STOP 1
   
   nullify (a4p, a8p)
   L44 = t4 .and. associated (a4p, a4)
   L84 = t8 .and. associated (a4p, a4)
   L48 = t4 .and. associated (a8p, a8)
   L88 = t8 .and. associated (a8p, a8)
   if (L44 .and. L84 .and. L48 .and. L88) STOP 2

   a4p => a4(1:10:2, 1:10:2)
   a8p => a8(1:4, 1:4)
   L44 = t4 .and. associated (a4p, a4(1:10:2, 1:10:2))
   L84 = t8 .and. associated (a4p, a4(1:10:2, 1:10:2))
   L48 = t4 .and. associated (a8p, a8(1:4, 1:4))
   L88 = t8 .and. associated (a8p, a8(1:4, 1:4))
   if (.not. (L44 .and. L84 .and. L48 .and. L88)) STOP 3
end

