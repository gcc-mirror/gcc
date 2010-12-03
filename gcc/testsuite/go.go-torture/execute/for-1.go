package main

func main() {
  sum := 0;
  for i := 0; i < 10; i++ {
    sum += i;
  }
  if sum != 45 { panic(0) }
}
