! PR 13919, segfault when file is empty
      open(unit=8,file='/dev/null')
      read(8,*,end=1)i
1     continue
      end
