// PR 97460
// ICE, null dereference

class io_context {
  template <int> class basic_executor_type;
};
template <int> class io_context::basic_executor_type {
  template <int> friend class basic_executor_type;
};
