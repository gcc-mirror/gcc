        program pr3743
c On mips-sgi-irix6.5 get
c
c         i3 = ISHFT(i,BIT_SIZE(i))
c              ^
c Reference to intrinsic `ISHFT' at (^) invalid -- 
c one or more arguments have incorrect type
c
c     David Billinghurst <David.Billinghurst@riotinto.com>
c
      integer   i, i2, i3
      i  = 3
      i2 = BIT_SIZE(i)
      i3 = ISHFT(i,i2)
      i3 = ISHFT(i,BIT_SIZE(i))
      end
