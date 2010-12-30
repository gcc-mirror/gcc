package main

func main() {
  ch1 := make(chan int);
  ch2 := make(chan int);
  go func (ch1, ch2 chan int) { ch1 <- 1; ch2 <- 2; } (ch1, ch2);
  count := 0;
  var v int;
  for count != 2 {
      select
	{
	case v := <- ch1:
	  if v != 1 {
	    panic(0)
	  }
	  count++

	case v = <- ch2:
	  if v != 2 {
	    panic(1)
	  }
	  count++
	}
    }
  if v != 2 {
    panic(2)
  }
}
