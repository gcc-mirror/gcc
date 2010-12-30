package main

func main() {
  c := make(chan int, 1);
  c <- 0;
  if <-c != 0 { panic(0) }
}
