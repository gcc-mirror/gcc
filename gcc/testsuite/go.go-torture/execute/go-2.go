package main

func send_one(c chan <- int, val int) {
  c <- val;
}

func main() {
  c := make(chan int);
  go send_one(c, 0);
  if <-c != 0 { panic(0) }
}
