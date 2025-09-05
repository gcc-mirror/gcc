// { dg-require-effective-target shared }
// { dg-require-effective-target lto }
// { dg-options "-O2 -flto -shared" }

struct QAbstractFileEngine {
  virtual bool seek(long long);
};
struct QQmlPreviewFileEngine : QAbstractFileEngine {
  bool seek(long long);
  int m_contents;
  QAbstractFileEngine * m_fallback;
};
bool f(void*);
bool QQmlPreviewFileEngine::seek(long long newPos) {
  if (m_fallback)
    return  m_fallback->seek(newPos);
  else
    return f(&m_contents);
}
