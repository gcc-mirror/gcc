// { dg-do compile }
struct QString {};
typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
 QString & sprintf(QString &s,const QString &szFmt,...)
 {
  va_list list;
  __builtin_va_start(list,((const char *)(&(szFmt))));
  __builtin_va_end(list);
  return s;
 }
