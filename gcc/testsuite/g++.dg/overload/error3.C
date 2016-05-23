// PR c++/42701
// Test for error-recovery on code that is ill-formed by DR 147.

namespace Glib {
    class ustring {
    public:
        typedef unsigned size_type;
        ustring(const char* src, size_type n);
        ustring(const char* src);
    };
}
namespace Gdk {
    class Color {
    public:
        explicit Color(const Glib::ustring& value);
    };
}
namespace Gtk {
    enum StateType { STATE_NORMAL };
    class Widget   {
    public:
        void modify_bg(StateType state, const Gdk::Color& color);
    };
    class Label {
    public:
        void set_text(const Glib::ustring &str);
    };
}
typedef enum Result { eSuccess = 0 } Result;
class MainWindow  {
    void update_status(Result result);
    Gtk::Widget status_frame;
    Gtk::Label status_label;
};
void MainWindow::update_status(Result result) {
    switch (result) {
        status_frame.modify_bg(Gtk::STATE_NORMAL,Gdk::Color::Color("green")); // { dg-error "" }
	// { dg-warning "statement will never be executed" "" { target *-*-* } 37 }
        status_frame.modify_bg(Gtk::STATE_NORMAL,Gdk::Color::Color("red")); // { dg-error "" }
        status_label.set_text("Out of memory");
    }
}
