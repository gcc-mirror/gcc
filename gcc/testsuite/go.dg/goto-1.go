// { dg-do compile }

package main

func main() {
  goto lab;	// { dg-error "undefined label" }
}
