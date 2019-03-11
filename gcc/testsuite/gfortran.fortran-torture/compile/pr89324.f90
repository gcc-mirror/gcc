module a
contains
  pure function myotherlen()
    myotherlen = 99
  end  
  subroutine b
    characterx
    block
       character(myotherlen()) c
       c = "abc"
       x = c
    end block
  
    end  
end
