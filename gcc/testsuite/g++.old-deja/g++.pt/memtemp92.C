// Build don't link:
// Origin: "Adam J. Richter" <adam@yggdrasil.com>

template <class Style, class Base>
class theme_map {
};

class QWidget {
protected:
  virtual void *harmony_get_list_for_signal(const char *) const;

public:
  static theme_map<int, QWidget> ContentsThemes;

protected:
  virtual void updateDrawingObjects (void)
    {
      update_dro (QWidget::ContentsThemes);
    }

  template <class S, class B>
  void update_dro (theme_map<S, B>& themes)
    {
    }
};

void *QWidget::harmony_get_list_for_signal(const char *sig) const
{
  	return 0;
}
