package main

func send_one(c chan <- int) {
  c <- 0;
}

func main() {
  c := make(chan int);
  go send_one(c);
  if <-c != 0 { panic(0) }
}
