// PR c++/13971

struct QChar {
    static const QChar null;
};
struct QCharRef {
    operator QChar() const;
};
struct QString {
    QCharRef operator[](int i);
};

QChar fillParagraph(QString s, int psi) {
    return psi ? QChar::null : s[psi];
}

