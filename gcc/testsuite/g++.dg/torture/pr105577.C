// { dg-do compile }
// { dg-additional-options "-fexceptions -fnon-call-exceptions" }

namespace {
typedef __SIZE_TYPE__ size_t;
}
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT64_TYPE__ uint64_t;
namespace {
template <typename _Tp, _Tp __v> struct integral_constant {
  static constexpr _Tp value = __v;
};
template <bool __v> using __bool_constant = integral_constant<bool, __v>;
template <bool> struct __conditional {
  template <typename _Tp, typename> using type = _Tp;
};
template <bool _Cond, typename _If, typename _Else>
using __conditional_t = typename __conditional<_Cond>::type<_If, _Else>;
template <typename...> struct __and_;
template <typename _B1, typename _B2>
struct __and_<_B1, _B2> : __conditional_t<_B1::value, _B2, _B1> {};
template <typename> struct __not_ : __bool_constant<!bool()> {};
template <typename _Tp>
struct __is_constructible_impl : __bool_constant<__is_constructible(_Tp)> {};
template <typename _Tp>
struct is_default_constructible : __is_constructible_impl<_Tp> {};
template <typename _Tp> struct remove_extent { typedef _Tp type; };
template <bool> struct enable_if;
} // namespace
namespace std {
template <typename _Tp> struct allocator_traits { using pointer = _Tp; };
template <typename _Alloc> struct __alloc_traits : allocator_traits<_Alloc> {};
template <typename, typename _Alloc> struct _Vector_base {
  typedef typename __alloc_traits<_Alloc>::pointer pointer;
  struct {
    pointer _M_finish;
    pointer _M_end_of_storage;
  };
};
template <typename _Tp, typename _Alloc = _Tp>
class vector : _Vector_base<_Tp, _Alloc> {
public:
  _Tp value_type;
  typedef size_t size_type;
};
template <typename _Tp, typename _Dp> class __uniq_ptr_impl {
  template <typename _Up, typename> struct _Ptr { using type = _Up *; };

public:
  using _DeleterConstraint =
      enable_if<__and_<__not_<_Dp>, is_default_constructible<_Dp>>::value>;
  using pointer = typename _Ptr<_Tp, _Dp>::type;
};
template <typename _Tp, typename _Dp = _Tp> class unique_ptr {
public:
  using pointer = typename __uniq_ptr_impl<_Tp, _Dp>::pointer;
  pointer operator->();
};
enum _Lock_policy { _S_atomic } const __default_lock_policy = _S_atomic;
template <_Lock_policy = __default_lock_policy> class _Sp_counted_base;
template <typename, _Lock_policy = __default_lock_policy> class __shared_ptr;
template <_Lock_policy> class __shared_count { _Sp_counted_base<> *_M_pi; };
template <typename _Tp, _Lock_policy _Lp> class __shared_ptr {
  using element_type = typename remove_extent<_Tp>::type;
  element_type *_M_ptr;
  __shared_count<_Lp> _M_refcount;
};
template <typename _Tp> class shared_ptr : __shared_ptr<_Tp> {
public:
  shared_ptr() noexcept : __shared_ptr<_Tp>() {}
};
enum CompressionType : char;
class SliceTransform;
enum Temperature : uint8_t;
struct MutableCFOptions {
  MutableCFOptions()
      : soft_pending_compaction_bytes_limit(),
        hard_pending_compaction_bytes_limit(level0_file_num_compaction_trigger),
        level0_slowdown_writes_trigger(level0_stop_writes_trigger),
        max_compaction_bytes(target_file_size_base),
        target_file_size_multiplier(max_bytes_for_level_base),
        max_bytes_for_level_multiplier(ttl), compaction_options_fifo(),
        min_blob_size(blob_file_size), blob_compression_type(),
        enable_blob_garbage_collection(blob_garbage_collection_age_cutoff),
        max_sequential_skip_in_iterations(check_flush_compaction_key_order),
        paranoid_file_checks(bottommost_compression), bottommost_temperature(),
        sample_for_compression() {}
  shared_ptr<SliceTransform> prefix_extractor;
  uint64_t soft_pending_compaction_bytes_limit;
  uint64_t hard_pending_compaction_bytes_limit;
  int level0_file_num_compaction_trigger;
  int level0_slowdown_writes_trigger;
  int level0_stop_writes_trigger;
  uint64_t max_compaction_bytes;
  uint64_t target_file_size_base;
  int target_file_size_multiplier;
  uint64_t max_bytes_for_level_base;
  double max_bytes_for_level_multiplier;
  uint64_t ttl;
  vector<int> compaction_options_fifo;
  uint64_t min_blob_size;
  uint64_t blob_file_size;
  CompressionType blob_compression_type;
  bool enable_blob_garbage_collection;
  double blob_garbage_collection_age_cutoff;
  uint64_t max_sequential_skip_in_iterations;
  bool check_flush_compaction_key_order;
  bool paranoid_file_checks;
  CompressionType bottommost_compression;
  Temperature bottommost_temperature;
  uint64_t sample_for_compression;
};
template <class T, size_t kSize = 8> class autovector {
  using value_type = T;
  using size_type = typename vector<T>::size_type;
  size_type buf_[kSize * sizeof(value_type)];
};
class MemTable;
class ColumnFamilyData;
struct SuperVersion {
  MutableCFOptions write_stall_condition;
  autovector<MemTable *> to_delete;
};
class ColumnFamilySet {
public:
  class iterator {
  public:
    iterator operator++();
    bool operator!=(iterator);
    ColumnFamilyData *operator*();
    ColumnFamilyData *current_;
  };
  iterator begin();
  iterator end();
};
class VersionSet {
public:
  ColumnFamilySet *GetColumnFamilySet();
};
struct SuperVersionContext {
  void NewSuperVersion() { new SuperVersion(); }
};
class DBImpl {
  unique_ptr<VersionSet> versions_;
  void InstallSuperVersionAndScheduleWork(ColumnFamilyData *,
                                          SuperVersionContext *,
                                          const MutableCFOptions &);
};
void DBImpl::InstallSuperVersionAndScheduleWork(ColumnFamilyData *,
                                                SuperVersionContext *sv_context,
                                                const MutableCFOptions &) {
  sv_context->NewSuperVersion();
  for (auto my_cfd : *versions_->GetColumnFamilySet())
    ;
}
} // namespace std
