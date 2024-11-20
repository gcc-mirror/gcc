/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

void f1 (void* p, int arr[]);
#pragma omp declare variant (f1) match (construct={dispatch}) adjust_args (need_device_ptr: p, arr)
void f2 (void* p, int arr[]);

void test (void)
{
  void *p;
  int arr[2];

/* Note there are multiple matches because every variable capturing matches in addition,
   i.e. scan-tree-dump-times = 1 plus number of captures used for backward references.

   For the first scan-tree-dump, on some targets the __builtin_omp_get_mapped_ptr get
   swapped. As the counting does not work well with variable-name capturing, it only
   uses 'scan-tree-dump' and not 'scan-tree-dump-times'.  */

#pragma omp dispatch
/* { dg-final { scan-tree-dump "#pragma omp dispatch\[ \t\n\r\{]*int (D\.\[0-9]+);\[ \t\n\r]*void \\* (D\.\[0-9]+);\[ \t\n\r]*void \\* (D\.\[0-9]+);\[ \t\n\r]*\\1 = __builtin_omp_get_default_device \\(\\);\[ \t\n\r]*\\2 = __builtin_omp_get_mapped_ptr \\(&arr, \\1\\);\[ \t\n\r]*\\3 = __builtin_omp_get_mapped_ptr \\(p, \\1\\);\[ \t\n\r]*f1 \\(\\3, \\2\\);|#pragma omp dispatch\[ \t\n\r\{]*int (D\.\[0-9]+);\[ \t\n\r]*void \\* (D\.\[0-9]+);\[ \t\n\r]*void \\* (D\.\[0-9]+);\[ \t\n\r]*\\4 = __builtin_omp_get_default_device \\(\\);\[ \t\n\r]*\\5 = __builtin_omp_get_mapped_ptr \\(p, \\4\\);\[ \t\n\r]*\\6 = __builtin_omp_get_mapped_ptr \\(&arr, \\4\\);\[ \t\n\r]*f1 \\(\\5, \\6\\);" "gimple" } } */
  f2 (p, arr);
#pragma omp dispatch is_device_ptr(p)
/* { dg-final { scan-tree-dump-times "#pragma omp dispatch is_device_ptr\\(p\\)\[ \t\n\r\{]*int (D\.\[0-9]+);\[ \t\n\r]*void \\* (D\.\[0-9]+);\[ \t\n\r]*\\1 = __builtin_omp_get_default_device \\(\\);\[ \t\n\r]*\\2 = __builtin_omp_get_mapped_ptr \\(&arr, \\1\\);\[ \t\n\r]*f1 \\(p, \\2\\);" 3 "gimple" } } */
  f2 (p, arr);
#pragma omp dispatch is_device_ptr(arr)
/* { dg-final { scan-tree-dump-times "#pragma omp dispatch is_device_ptr\\(arr\\)\[ \t\n\r\{]*int (D\.\[0-9]+);\[ \t\n\r]*void \\* (D\.\[0-9]+);\[ \t\n\r]*\\1 = __builtin_omp_get_default_device \\(\\);\[ \t\n\r]*\\2 = __builtin_omp_get_mapped_ptr \\(p, \\1\\);\[ \t\n\r]*f1 \\(\\2, &arr\\);" 3 "gimple" } } */
  f2 (p, arr);
#pragma omp dispatch is_device_ptr(p, arr)
/* { dg-final { scan-tree-dump-times "#pragma omp dispatch is_device_ptr\\(arr\\) is_device_ptr\\(p\\)\[ \t\n\r\{]*f1 \\(p, &arr\\);" 1 "gimple" } } */
  f2 (p, arr);
}


