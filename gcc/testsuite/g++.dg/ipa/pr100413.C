/* { dg-do compile } */
/* { dg-options "-std=c++11 -O2 -fno-guess-branch-probability -fno-inline-functions-called-once -fipa-cp-clone -fipa-pta -fnon-call-exceptions --param=ipa-cp-eval-threshold=0" } */


template <typename> class allocator {
public:
  ~allocator();
};
template <typename> struct allocator_traits;
template <typename _Tp> struct allocator_traits<allocator<_Tp>> {
  using allocator_type = allocator<_Tp>;
  template <typename _Up> using rebind_alloc = allocator<_Up>;
  static void deallocate(allocator_type);
};
template <typename _ForwardIterator, typename _Tp>
void _Destroy(_ForwardIterator, _ForwardIterator, _Tp);
struct __alloc_traits : allocator_traits<allocator<int>> {
  struct rebind {
    typedef rebind_alloc<int> other;
  };
};
struct _Vector_base {
  struct _Vector_impl_data {
    int _M_start;
    int _M_finish;
  };
  struct _Vector_impl : __alloc_traits::rebind::other, _Vector_impl_data {};
  __alloc_traits::rebind::other _M_get_Tp_allocator();
  ~_Vector_base() { _M_deallocate(); }
  _Vector_impl _M_impl;
  void _M_deallocate() { __alloc_traits::deallocate(_M_impl); }
};
class vector : _Vector_base {
public:
  vector() noexcept {
    allocator<int> __trans_tmp_1 = _M_get_Tp_allocator();
    _Destroy(_M_impl._M_start, _M_impl._M_finish, __trans_tmp_1);
  }
  void size();
};
struct HTTPCallback {
  virtual void OnFailure();
};
struct ContentCallback {
  virtual void OnDownloadProgress();
};
class ClientNetworkContentSocketHandler : ContentCallback, HTTPCallback {
  vector requested;
  vector infos;
  vector lastActivity;
  void OnFailure();
public:
  int IDLE_TIMEOUT = 0;
  ClientNetworkContentSocketHandler();
  void DownloadSelectedContent();
} _network_content_client;
void ClientNetworkContentSocketHandler::DownloadSelectedContent() {
  vector content;
  content.size();
}
void ClientNetworkContentSocketHandler::OnFailure() {
  DownloadSelectedContent();
}
ClientNetworkContentSocketHandler::ClientNetworkContentSocketHandler() {}
