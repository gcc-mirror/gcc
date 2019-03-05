      function doesntwork_p8(c,a1,a2,a3,a4,a5,a6)
        implicit none
        character(len=20) :: doesntwork_p8
        character :: c
        integer :: a1,a2,a3,a4,a5,a6
        if (a1 /= 1 .or. a2 /= 2 .or. a3 /= 3 .or. a4 /= 4 .or. a5 /= 5
     &       .or. a6 /= 6) stop 1
       if (c /= 'o ') stop 2
       doesntwork_p8 = 'foo'
       return
       end
