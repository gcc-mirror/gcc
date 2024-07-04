int main() {
  char a[20] = "ab";
  __builtin_strcpy(a, "123");
  long n0 = __builtin_strlen(a);
  __builtin_strncpy(a + 3, a, n0);
  __builtin_strlen(a);
  return 0;
}
