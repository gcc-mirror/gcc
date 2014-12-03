// PR10032
// { dg-options "-pedantic" }

int main() {
  goto label;   // { dg-message "" }
  
  int temp = 1; // { dg-message "" } 
  
  label:        // { dg-error "" } 
    return 1;
}
