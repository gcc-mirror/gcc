// Check the suppression of -Wuse-after-free for destructors on ARM
// { dg-do compile }
// { dg-options "-Wuse-after-free" }

struct range_label {
  virtual ~range_label();
};

struct unpaired_bidi_rich_location {
  struct custom_range_label : range_label {};
  unpaired_bidi_rich_location(int);
  custom_range_label m_custom_label;
};

void maybe_warn_bidi_on_close() { unpaired_bidi_rich_location(0); }
