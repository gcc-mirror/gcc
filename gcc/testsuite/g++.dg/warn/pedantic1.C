// PR10032
// { dg-options "-pedantic" }

int main() {
  goto label;   // { dg-error "" }
  
  int temp = 1; // { dg-error "" } 
  
  label:        // { dg-error "" } 
    return 1;
}
