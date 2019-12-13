// { dg-do assemble  }
int main() {
  int* d;
  dynamic_cast<void*>(d);	// { dg-error "3:cannot .dynamic_cast." } 
}
