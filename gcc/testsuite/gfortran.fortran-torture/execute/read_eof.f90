! PR 13919, segfault when file is empty
      open(unit=8,status='scratch')
      read(8,*,end=1)i
      STOP 1
1     continue
      end
